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
%%% Created : 19 Sep 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_hook_mgr).
-compile({parse_transform, lager_transform}).

-include("occi.hrl").

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {name :: atom(),
		m    :: atom(), 
		f    :: atom()}).

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
-spec start_link(atom(), {atom(), atom()}, reference()) -> 
			{ok, pid()} 
			    | ignore 
			    | {error, term()}.
start_link(Name, {Mod, Fun}, Ref) ->
    gen_server:start_link({local, Ref}, ?MODULE, {Name, {Mod, Fun}}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({Name, {Mod, Fun}}) ->
    {ok, #state{name=Name, m=Mod, f=Fun}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, From, S) ->
    hook(Request, From, S).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, S) ->
    case hook(Msg, undefined, S) of
	{reply, _Reply, S2} -> {noreply, S2};
	{reply, _Reply, S2, Timeout} -> {noreply, S2, Timeout};
	{noreply, S2} -> {noreply, S2};
	{noreply, S2, Timeout} -> {noreply, S2, Timeout};
	{stop, Reason, _Reply, S2} -> {stop, Reason, S2};
	{stop, Reason, S2} -> {stop, Reason, S2}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%
% @doc generic function for cast and call
%
hook({on_save, Entity}, _From, S) ->
    Mod = S#state.m,
    Fun = S#state.f,
    logger:info("<hook:on_save> ~p:~p(~p)~n", [Mod, Fun, Entity]),
    Reply = Mod:Fun(Entity),
    {reply, Reply, S};
hook({on_update, {Old, New}}, _From, S) ->
    Mod = S#state.m,
    Fun = S#state.f,
    logger:info("<hook:on_update> ~p:~p(~p, ~p)~n", [Mod, Fun, Old, New]),
    Reply = Mod:Fun(Old, New),
    {reply, Reply, S};
hook({on_delete, Entity}, _From, S) ->
    Mod = S#state.m,
    Fun = S#state.f,
    logger:info("<hook:on_delete> ~p:~p(~p)~n", [Mod, Fun, Entity]),
    Reply = Mod:Fun(Entity),
    {reply, Reply, S};
hook({on_action, Entity, Action}, _From, S) ->
    Mod = S#state.m,
    Fun = S#state.f,
    logger:info("<hook:on_action> ~p:~p(~p, ~p)~n", [Mod, Fun, Entity, Action]),
    Reply = Mod:Fun(Entity, Action),
    {reply, Reply, S};
hook(Request, _From, S) ->
    logger:error("Unknown hook: ~p~n", [Request]),
    {reply, error, S}.
