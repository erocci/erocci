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

-include("occi.hrl").

%% API
-export([start_link/0,
	 loop/0]).
-export([register/1,
	 trigger/2]).

-define(SUPERVISOR, ?MODULE).
-define(TABLE, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> ok.
start_link() ->
    Pid = spawn_link(fun init/0),
    {ok, Pid}.

-spec register(hook_type()) -> ok | {error, term()}.
register({pid, Pid}) ->
    ?info("Registering hook handler: {pid, ~p}~n", [Pid]),
    add_handler({pid, Pid}).

-spec trigger(occi_node(), occi_action()) -> {true, occi_node()} 
						 | false
						 | {error, term()}.
trigger(#occi_node{id=Id}=Node, #occi_action{id=_ActionId}=Action) ->
    case occi_action:check(Action) of
	ok ->
	    ?info("Trigger action: ~p~n", [occi_uri:to_string(Id)]),
	    send_all(Action, Node);
	{error, Err} ->
	    ?error("Error triggering action: ~p~n", [Err]),
	    {error, Err}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init() ->
    ?info("Starting OCCI hooks manager"),
    ?TABLE = ets:new(?TABLE, [set, public, {keypos, 1}, named_table]),
    erlang:hibernate(?MODULE, loop, []).
    
loop() ->
    receive 
	stop ->
	    exit(normal);
	_ ->
	    erlang:hibernate(?MODULE, loop, [])
    end.

add_handler(Handler) ->
    Handlers = case ets:lookup(?TABLE, handlers) of
		   [] ->
		       {handlers, [Handler]};
		   [{handlers, H}] ->
		       {handlers, [Handler|H]}
	       end,
    ets:insert(?TABLE, Handlers),
    ok.

get_handlers() ->
    case ets:lookup(?TABLE, handlers) of
	[] -> [];
	[{handlers, Handlers}] -> Handlers
    end.

send_all(Evt, Node) ->
    send(Evt, {false, Node}, get_handlers()).

send(_Evt, {false, _Node}, []) ->
    false;
send(_Evt, {true, Node}, []) ->
    {true, Node};
send(_Evt, {error, Err}, []) ->
    {error, Err};
send(Evt, {Updated, Node}, [{pid, Pid}|Tail]) ->
    Pid ! {self(), Evt, Node},
    receive 
	{true, Node2} ->
	    send(Evt, {true, Node2}, Tail);
	false ->
	    send(Evt, {Updated, Node}, Tail);
	{error, Err} ->
	    {error, Err}
    after 1000 ->
	    {error, timeout}
    end.
