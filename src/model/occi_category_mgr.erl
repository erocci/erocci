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
-export([get/1,
	 register_extension/2,
	 register_kind/1,
	 register_mixin/1,
	 register_user_mixin/1,
	 register_action/1,
	 get_categories/0,
	 get_actions/0]).

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

-spec get(occi_cid()) -> occi_category().
get(#occi_cid{class=kind}=Cid) ->
    case ets:match_object(?TABLE, {occi_kind, Cid, '_', '_', '_', '_', '_', '_'}) of
	[] -> undefined;
	[Kind] -> Kind
    end;
get(#occi_cid{class=mixin}=Cid) ->
    case ets:match_object(?TABLE, {occi_mixin, Cid, '_', '_', '_', '_', '_', '_', '_'}) of
	[] -> undefined;
	[Mixin] -> Mixin
    end.

register_extension({xml, Path}, Mapping) ->
    case occi_parser_xml:load_extension(Path) of
	{error, parse_error} ->
	    {error, parse_error};
	Ext ->
	    lists:foreach(fun(#occi_kind{id=Id}=Kind) ->
				  {Uri, Backend} = get_mapping(Id, Mapping),
				  register_kind(Kind#occi_kind{location=Uri, backend=Backend});
			     (#occi_mixin{id=Id}=Mixin) ->
				  {Uri, Backend} = get_mapping(Id, Mapping),
				  register_mixin(Mixin#occi_mixin{location=Uri, backend=Backend})
			  end,
			  occi_extension:get_categories(Ext))
    end.

register_kind(#occi_kind{id=Id, location=Uri}=Kind) ->
    lager:info("Registering kind: ~s~s -> ~s~n", 
	       [ Id#occi_cid.scheme, Id#occi_cid.term, Uri ]),
    ets:insert(?TABLE, Kind),
    lists:foreach(fun(Action) ->
			  register_action(Action)
		  end,
		  occi_kind:get_actions(Kind)).

register_mixin(#occi_mixin{id=Id, location=Uri}=Mixin) ->
    lager:info("Registering mixin: ~s~s -> ~s~n", 
	       [ Id#occi_cid.scheme, Id#occi_cid.term, Uri ]),
    ets:insert(?TABLE, Mixin),
    lists:foreach(fun(Action) ->
			  register_action(Action)
		  end,
		  occi_mixin:get_actions(Mixin)).

register_user_mixin(#occi_mixin{id=Id, location=Uri}=Mixin) ->
    lager:info("Registering mixin: ~s~s -> ~s~n", [Id#occi_cid.scheme, Id#occi_cid.term, Uri]),
    occi_store:create(Mixin).

register_action(Action) ->
    Id = Action#occi_action.id,
    lager:info("Registering action: ~s~s~n", 
	       [ Id#occi_cid.scheme, Id#occi_cid.term ]),
    ets:insert(?TABLE, Action).

-spec get_categories() -> [occi_category()].
get_categories() ->
    Categories = ets:match_object(?TABLE, {occi_kind, '_', '_', '_', '_', '_', '_', '_'}),
    Categories ++ ets:match_object(?TABLE, {occi_mixin, '_', '_', '_', '_', '_', '_', '_', '_'}).

-spec get_actions() -> [occi_action()].
get_actions() ->
    ets:match_object(?TABLE, {occi_action, '_', '_', '_', '_'}).

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
get_mapping(Id, Mapping) ->
    case catch dict:find(Id, Mapping) of
	error ->
	    lager:error("Unmapped category: ~s~s~n", [Id#occi_cid.scheme, 
						      Id#occi_cid.term]),
	    throw({unmapped_category, Id});
	{ok, {Uri, Backend}} ->
	    {Uri, Backend}
    end.
