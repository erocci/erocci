%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% 
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%% 
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%% 
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_store).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0, register_category/2, get_backend/1]).
-export([get_categories/0,
	 is_valid_path/1,
	 load/1]).

%% supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

-record(store_obj, {uri                       :: uri(),
		    type      = occi_category :: occi_category | occi_entity,
		    backend                   :: atom(),
		    id                        :: occi_cid(),
		    mod                       :: atom()}).
-type(store_obj() :: #store_obj{}).
-export_type([store_obj/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Ret = supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []),
    occi_backend:start_backends(),
    Ret.

-spec register_category(atom(), occi_backend:backend_category()) -> ok.
register_category(Ref, {Mod, Uri}) ->
    Trans = fun() -> 
		    Id = occi_type:get_id(Mod),
		    Type = #store_obj{uri=Uri, backend=Ref, id=Id, mod=Mod},
		    lager:info("Registering ~p at ~s~n", [occi_renderer:to_uri(Id), Uri]),
		    mnesia:write(Type)
	    end,
    mnesia:transaction(Trans).

-spec get_backend(occi_cid()) -> pid().
get_backend(Id) ->
    case mnesia:dirty_match_object(#store_obj{id=Id, _='_'}) of
	[ #store_obj{backend=Ref} ] -> Ref;
	[] ->
	    lager:error("No backend associated to this category: ~p ~n", [Id]),
	    error
    end.

-spec get_categories() -> [occi_category()].
get_categories() ->
    Types = mnesia:dirty_match_object(#store_obj{_ ='_'}),
    BaseUrl = occi_config:get(base_location),
    Categories = [ occi_type:get_category(BaseUrl, Uri, Mod) || 
		     #store_obj{mod=Mod, uri=Uri} <- Types ],
    Actions = [ occi_type:get_actions(Mod) || #store_obj{mod=Mod} <- Types ],
    lists:flatten([Categories, Actions]).

-spec is_valid_path(Path :: uri()) -> false | store_obj().
is_valid_path(Path) ->
    case mnesia:dirty_match_object(#store_obj{uri=Path, _='_'}) of
	[] ->
	    false;
	[Obj] ->
	    Obj
    end.

-spec load(store_obj()) -> occi_category() | occi_entity().
load(#store_obj{uri=Path, type=occi_category}) ->
    case mnesia:dirty_read(store_obj, Path) of
	[] ->
	    throw({error, einval, Path});
	[#store_obj{mod=Mod, uri=Uri}] ->
	    BaseUrl = occi_config:get(base_location),
	    occi_type:get_category(BaseUrl, Uri, Mod)
    end;
load(_) ->
    [].

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init([]) ->
    lager:info("Starting occi store"),
    mnesia:create_table(store_obj,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, store_obj)}]),
    mnesia:wait_for_tables(store_obj, infinite),
    % start no child, will be added with backends
    {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% internals
%%%===================================================================
