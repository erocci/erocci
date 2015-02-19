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
-module(occi_listener).

-behaviour(supervisor).

-include("occi.hrl").

%% API
-export([start_link/0, 
	 register/1]).

-type opts() :: [{atom(), any()}].
-callback start_link(atom(), opts()) -> ok | {error, atom()}.
-callback terminate(atom(), term()) -> ok.

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

-spec register({Ref :: atom(), Module :: atom(), Opts :: term()}) -> {ok, pid()} | {error, term()}.
register({Ref, Module, Opts}) ->
    ChildSpec = {Ref,
		 {Module, start_link, [Ref, Opts]},
		 permanent,
		 2000,
		 worker,
		 [Module]},
    case supervisor:start_child(?SUPERVISOR, ChildSpec) of
	{ok, Pid} ->
	    {ok, Pid};
	{error, Err} ->
	    throw({error, Err});
	Else ->
	    throw({error, {invalid_value, Else}})
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
    ?info("Starting OCCI listeners manager"),
    {ok, {{one_for_one, 1000, 6000}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
