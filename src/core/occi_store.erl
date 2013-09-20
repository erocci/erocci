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
-export([start_link/0, add_category/2, add_backend/2]).
-export([get_categories/0,
	 is_valid_path/1,
	 load/1]).

%% supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

-record(store_mountpoint, {uri :: uri(), backend :: atom()}).
-type(store_mountpoint() :: #store_mountpoint{}).

-record(store_category, {id :: #occi_cid{}, mod :: atom(), url :: uri()}).
-type(store_category() :: #store_category{}).

-type(backend_desc() :: {atom(), atom(), term()}).

-export_type([store_mountpoint/0]).

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
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

-spec add_category(term(), [occi_hook:hook()]) -> ok.
add_category({mod, Mod, Location}, Hooks) ->
    Id = occi_type:get_id(Mod),
    lager:info("Registering category: ~p -> ~p~n", [Id, Location]),
    Trans = fun() -> 
		    Cat = #store_category{id=Id,
					  mod=Mod,
					  url=Location},
		    mnesia:write(Cat)
	    end,
    mnesia:transaction(Trans),    
    lists:foreach(fun(Hook) ->
			  occi_hook:add_hook(Id, Hook)
		  end, Hooks).

-spec add_backend(backend_desc(), uri()) ->
			 {ok, pid()} 
			     | ignore 
			     | {error, term()}.
add_backend({Ref, Mod, Opts}, Path) ->
    lager:info("Registering backend: ~p -> ~p~n", [Ref, Path]),
    Trans = fun() -> 
		    Mp = #store_mountpoint{uri=Path, backend=Ref},
		    mnesia:write(Mp)
	    end,
    mnesia:transaction(Trans),
    Backend = {Ref, {?MODULE, start_link, [Ref, Mod, [{ref, Ref} | Opts]]}, 
	       permanent, 5000, worker, [?MODULE, Mod]},
    supervisor:start_child(occi_store, Backend).

-spec get_categories() -> [occi_category()].
get_categories() ->
    Types = mnesia:dirty_match_object(#store_category{_ ='_'}),
    Categories = [ occi_type:get_category(Url, Mod) || 
		     #store_category{mod=Mod, url=Url} <- Types ],
    Actions = [ occi_type:get_actions(Mod) || #store_category{mod=Mod} <- Types ],
    lists:flatten([Categories, Actions]).

-spec is_valid_path(Path :: uri()) -> false | ok.
is_valid_path(_Path) ->
    ok.

-spec load(store_category()) -> occi_category() | occi_entity().
load(_) ->
    [].

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init([]) ->
    lager:info("Starting OCCI categories manager"),
    mnesia:create_table(store_mountpoint,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, store_mountpoint)}]),
    mnesia:create_table(store_category,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, store_category)}]),
    mnesia:wait_for_tables([store_mountpoint, store_category], 
			   infinite),
    % start no child, will be added with backends
    {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% internals
%%%===================================================================
