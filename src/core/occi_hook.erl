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
-export([add_hook/2, notify/3]).

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

-spec add_hook(occi_cid(), hook()) -> ok.
add_hook(Id, {Name, Fun}) ->
    Ref = register_hook(Id, Name),
    HookMgr = {Ref, {occi_hook_mgr, start_link, [Name, Fun, Ref]}, 
	       permanent, 5000, worker, [occi_hook_mgr]},
    supervisor:start_child(?MODULE, HookMgr).

notify(HookName, Id, Data) ->
    Managers = mnesia:dirty_match_object(#hook_rec{key={HookName, Id}, _ ='_'}),
    list:foreach(fun(Manager) ->
			 gen_server:cast(Manager#hook_rec.ref, {HookName, Data})
		 end, Managers).

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
    mnesia:create_table(hook_rec,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, hook_rec)}]),
    mnesia:wait_for_tables([hook_cat], infinite),
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Childs = [],

    {ok, {SupFlags, Childs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
register_hook(Id, Name) ->
    Ref = make_ref(),
    HookRec = #hook_rec{key={Name, Id}, ref=Ref},
    Trans = fun() -> 
		    mnesia:write(HookRec)
	    end,
    mnesia:transaction(Trans),
    HookRec.
