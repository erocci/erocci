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
%%% Created :  6 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_hook).
-compile({parse_transform, lager_transform}).

-include("occi.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([trigger/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

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
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

-spec trigger(occi_node(), occi_action()) -> {true, occi_node()} 
						 | false
						 | {error, term()}.
trigger(#occi_node{id=Id}=_Node, #occi_action{id=ActionId}=Action) ->
    case occi_action:check(Action) of
	ok ->
	    lager:info("Trigger action: ~p <- ~p~n", [occi_uri:to_string(Id), 
						      lager:pr(ActionId, ?MODULE)]),
	    false;
	{error, Err} ->
	    lager:error("Error triggering action: ~p~n", [Err]),
	    {error, Err}
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
init(_) ->
    lager:info("Starting OCCI hooks manager"),
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Childs = [],

    {ok, {SupFlags, Childs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
