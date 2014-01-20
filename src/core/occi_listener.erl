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
-compile({parse_transform, lager_transform}).

-behaviour(supervisor).

-include("occi.hrl").

%% API
-export([start_link/0, 
	 register/1,
	 add_collection/2]).

-type opts() :: [{atom(), any()}].
-callback start_link(atom(), opts()) -> ok | {error, atom()}.
-callback terminate(atom()) -> ok.
-callback add_collection(atom(), occi_category(), uri()) -> ok | {error, atom()}.

%% Supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).
-define(TABLE, ?MODULE).

-record(listener, {ref :: reference(), 
		   mod ::atom()}).

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

register({Ref, Module, Opts}) ->
    lager:info("Registering listener: ~p~n", [Module]),
    ChildSpec = {Ref,
		 {Module, start_link, [Ref, Opts]},
		 permanent,
		 brutal_kill,
		 worker,
		 [Module]},
    case supervisor:start_child(?SUPERVISOR, ChildSpec) of
	{ok, Pid} ->
	    ets:insert(?TABLE, #listener{ref=Ref, mod=Module}),
	    {ok, Pid};
	{error, Err} ->
	    {error, Err}
    end.

add_collection(Category, Uri) ->
    lists:foreach(fun (#listener{ref=Ref, mod=Mod}) ->
			  Mod:add_collection(Ref, Category, Uri)
		  end, get_listeners()).

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
    lager:info("Starting OCCI listeners manager"),
    ?TABLE = ets:new(?TABLE, [set, public, {keypos, 2}, named_table]),
    {ok, {{one_for_one, 1000, 6000}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_listeners() ->
    ets:match_object(?TABLE, #listener{_='_'}).
