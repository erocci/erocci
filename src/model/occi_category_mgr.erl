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
%%% Created : 27 Sep 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_category_mgr).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([find/1,
	 get/1,
	 register_extension/2,
	 register_kind/1,
	 register_mixin/1,
	 register_user_mixin/1,
	 register_action/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

register_extension({xml, Path}, Mapping) ->
    case occi_parser_xml:load_extension(Path) of
	{error, parse_error} ->
	    {error, parse_error};
	Ext ->
	    lists:foreach(fun(#occi_kind{id=Id}=Kind) ->
				  {Uri, Backend} = get_mapping(Id, Mapping),
				  register_kind(Kind#occi_kind{location=occi_config:get_url(Uri), backend=Backend});
			     (#occi_mixin{id=Id}=Mixin) ->
				  {Uri, Backend} = get_mapping(Id, Mapping),
				  register_mixin(Mixin#occi_mixin{location=occi_config:get_url(Uri), backend=Backend})
			  end,
			  occi_extension:get_categories(Ext))
    end.

register_kind(#occi_kind{id=Id, location=#uri{}=Uri}=Kind) ->
    lager:info("Registering kind: ~s~s -> ~s~n", 
	       [ Id#occi_cid.scheme, Id#occi_cid.term, Uri#uri.path]),
    ets:insert(?TABLE, Kind),
    occi_listener:add_collection(Kind, Uri),
    lists:foreach(fun(Action) ->
			  register_action(Action)
		  end,
		  occi_kind:get_actions(Kind)).

register_mixin(#occi_mixin{id=Id, location=Uri}=Mixin) ->
    lager:info("Registering mixin: ~s~s -> ~s~n", 
	       [ Id#occi_cid.scheme, Id#occi_cid.term, Uri#uri.path ]),
    ets:insert(?TABLE, Mixin),
    occi_listener:add_collection(Mixin, Uri),
    lists:foreach(fun(Action) ->
			  register_action(Action)
		  end,
		  occi_mixin:get_actions(Mixin)).

register_user_mixin(#occi_mixin{id=Id, location=Uri}=Mixin) ->
    lager:info("Registering mixin: ~s~s -> ~s~n", [Id#occi_cid.scheme, Id#occi_cid.term, Uri#uri.path]),
    Backend = occi_store:get_backend(Uri),
    Mixin2 = Mixin#occi_mixin{backend=Backend},
    case occi_store:save(Mixin2) of
	ok ->
	    occi_listener:add_collection(Mixin2, Uri);
	{error, Err} ->
	    {error, Err}
    end.

register_action(Action) ->
    Id = Action#occi_action.id,
    lager:info("Registering action: ~s~s~n", 
	       [ Id#occi_cid.scheme, Id#occi_cid.term ]),
    ets:insert(?TABLE, Action).

-spec find(occi_cid()) -> [occi_category()].
find(#occi_cid{class=kind}=Cid) ->
    ets:match_object(?TABLE, #occi_kind{id=Cid, _='_'});
find(#occi_cid{class=mixin}=Cid) ->
    Mixin = #occi_mixin{id=Cid, _='_'},
    Mixins = ets:match_object(?TABLE, Mixin),
    {ok, UMixins} = occi_store:find(Mixin),
    lists:flatten([Mixins, UMixins]);
find(#occi_cid{class=action}=Cid) ->
    ets:match_object(?TABLE, #occi_action{id=Cid, _='_'});
find(#occi_cid{}=Cid) ->
    find(Cid#occi_cid{class=kind})
	++ find(Cid#occi_cid{class=mixin})
	++ find(Cid#occi_cid{class=action}).

% This function does not use occi_store (no transaction needed)
-spec get(occi_cid()) -> occi_mixin().
get(#occi_cid{class=mixin}=Cid) ->
    case ets:match_object(?TABLE, #occi_mixin{id=Cid, _='_'}) of
	[Mixin] ->
	    Mixin;
	_ ->
	    throw({error, unknown_mixin})
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    lager:info("Starting OCCI categories manager"),
    ?TABLE = ets:new(?TABLE, 
		     [ordered_set, public, {keypos, 2}, named_table, {read_concurrency, true}]),
    {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_mapping(Id, []) ->
    lager:error("Unmapped category: ~s~s~n", [Id#occi_cid.scheme, 
					      Id#occi_cid.term]),
    throw({unmapped_category, Id});
get_mapping(Id, [{Id, {Uri, Backend}}|_Tail]) ->
    {Uri, Backend};
get_mapping(Id, [_H|Tail]) ->
    get_mapping(Id, Tail).
