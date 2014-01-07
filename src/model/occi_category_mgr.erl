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
	 register_category/4,
	 register_mixin/1,
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
get(#occi_cid{}=Cid) ->
    case ets:match_object(?TABLE, {occi_category, '_', Cid, '_', '_'}) of
	[] ->
	    undefined;
	[Entry] ->
	    Entry
    end.

register_extension({xml, Path}, Mapping) ->
    case occi_parser_xml:load_extension(Path) of
	{error, parse_error} ->
	    {error, parse_error};
	Ext ->
	    lists:foreach(fun(Cat) -> 
				  Id = occi_category:get_id(Cat),
				  case catch dict:find(Id, Mapping) of
				      error ->
					  lager:error("Unmapped category: ~s~s~n", [Id#occi_cid.scheme, 
										    Id#occi_cid.term]),
					  throw({unmapped_category, Id});
				      {ok, {Uri, Backend}} -> 				  
					  register_category(Id, Cat, Uri, Backend)
				  end
			  end,
			  occi_extension:get_categories(Ext))
    end.

register_category(Id, Cat, Uri, Backend) ->
    Id = Cat#occi_category.id,
    lager:info("Registering ~s: ~s~s -> ~s~n", 
	       [ Id#occi_cid.class, Id#occi_cid.scheme, Id#occi_cid.term, Uri ]),
    ets:insert(?TABLE, Cat#occi_category{location=Uri, backend=Backend}),
    lists:foreach(fun(Action) ->
			  register_action(Action)
		  end,
		  occi_category:get_actions(Cat)).

register_mixin(#occi_category{}=_Mixin) ->
    ok.

register_action(Action) ->
    Id = Action#occi_action.id,
    lager:info("Registering action: ~s~s~n", 
	       [ Id#occi_cid.scheme, Id#occi_cid.term ]),
    ets:insert(?TABLE, Action).

-spec get_categories() -> [occi_category()].
get_categories() ->
    ets:match_object(?TABLE, {occi_category, '_', '_', '_', '_'}).

-spec get_actions() -> [occi_action()].
get_actions() ->
    ets:match_object(?TABLE, {occi_action, '_', '_', '_'}).

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
		     [set, public, {keypos, 2}, named_table, {read_concurrency, true}]),
    {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
