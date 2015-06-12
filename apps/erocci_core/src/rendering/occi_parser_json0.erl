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
%%% @doc An event based JSON parser
%%%
%%% @end
%%% Created : 11 Dec 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_parser_json0).

-behaviour(gen_fsm).

-include("occi.hrl").
-include("occi_parser.hrl").

%% API
-export([start/1,
	 stop/1,
	 parse/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% JSON parsing states
-export([init/3,
	 array/3,
	 object/3,
	 members/3,
	 pair/3,
	 value/3,
	 elements/3,
	 eof/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec parse(parser(), binary()) -> parser_result().
parse(#parser{src=#parser{mod=Mod}=Src}, Data) ->
    Mod:parse(Src, Data).

start(Sink) ->
    Parser = #parser{mod=?MODULE, sink=Sink},
    case gen_fsm:start(?MODULE, Parser, []) of
	{ok, Pid} -> 
	    Src = occi_scanner_json:start(Parser#parser{id=Pid}),
	    Parser#parser{id=Pid, src=Src};
	Err -> 
	    ?error("Error starting json parser: ~p~n", [Err]),
	    throw(Err)
    end.

stop(#parser{id=Ref, src=Src}) ->
    occi_scanner_json:stop(Src),
    gen_fsm:send_all_state_event(Ref, stop).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init(#parser{}=Ctx) ->
    {ok, init, Ctx#parser{stack=[eof], state=#state{}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(stop, _, State) ->
    occi_parser:send_event(eof, ok, State),
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(stop, _, _, State) ->
    occi_parser:send_event(eof, ok, State),
    {stop, normal, State};
handle_sync_event(Event, _From, _StateName, State) ->
    occi_parser:parse_error(Event, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, StateName, State) ->
    ?debug("Terminate with reason ~p in state ~s [~p]", [Reason, StateName, State]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% JSON parsing states
init(#token{name=objBegin}=Token, _From, Ctx) ->
    occi_parser:send_event(Token, {reply, ok, object, Ctx}, Ctx);
init(#token{name=arrBegin}=Token, _From, Ctx) ->
    occi_parser:send_event(Token, {reply, ok, array, Ctx}, Ctx);
init(#token{}=Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

object(#token{name=objEnd}=Token, _From, #parser{stack=[Top|Stack]}=Ctx) ->
    occi_parser:send_event(Token, {reply, ok, Top, Ctx#parser{stack=Stack}}, Ctx);
object(#token{name=string, data=Val}, _From, Ctx) ->
    occi_parser:send_event(#token{name=key, data=list_to_binary(Val)}, {reply, ok, pair, Ctx}, Ctx);
object(#token{}=Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

array(#token{name=arrEnd}=Token, _From, #parser{stack=[Top|Stack]}=Ctx) ->
    occi_parser:send_event(Token, {reply, ok, Top, Ctx#parser{stack=Stack}}, Ctx);
array(#token{name=string, data=Val}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=list_to_binary(Val)}, {reply, ok, elements, Ctx}, Ctx);
array(#token{name=float, data=Val}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=Val}, {reply, ok, elements, Ctx}, Ctx);
array(#token{name=integer, data=Val}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=Val}, {reply, ok, elements, Ctx}, Ctx);
array(#token{name=true}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=true}, {reply, ok, elements, Ctx}, Ctx);
array(#token{name=false}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=false}, {reply, ok, elements, Ctx}, Ctx);
array(#token{name=null}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=null}, {reply, ok, elements, Ctx}, Ctx);
array(#token{name=arrBegin}=Token, _From, #parser{stack=Stack}=Ctx) ->
    occi_parser:send_event(Token, {reply, ok, array, Ctx#parser{stack=[elements|Stack]}}, Ctx);
array(#token{name=objBegin}=Token, _From, #parser{stack=Stack}=Ctx) ->
    occi_parser:send_event(Token, {reply, ok, object, Ctx#parser{stack=[elements|Stack]}}, Ctx);
array(#token{}=Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

pair(#token{name=colon}, _From, Ctx) ->
    {reply, ok, value, Ctx};
pair(#token{}=Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

value(#token{name=string, data=Val}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=list_to_binary(Val)}, {reply, ok, members, Ctx}, Ctx);
value(#token{name=float, data=Val}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=Val}, {reply, ok, members, Ctx}, Ctx);
value(#token{name=integer, data=Val}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=Val}, {reply, ok, members, Ctx}, Ctx);
value(#token{name=true}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=true}, {reply, ok, members, Ctx}, Ctx);
value(#token{name=false}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=false}, {reply, ok, members, Ctx}, Ctx);
value(#token{name=null}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=null}, {reply, ok, members, Ctx}, Ctx);
value(#token{name=arrBegin}=Token, _From, #parser{stack=Stack}=Ctx) ->
    occi_parser:send_event(Token, {reply, ok, array, Ctx#parser{stack=[members|Stack]}}, Ctx);
value(#token{name=objBegin}=Token, _From, #parser{stack=Stack}=Ctx) ->
    occi_parser:send_event(Token, {reply, ok, object, Ctx#parser{stack=[members|Stack]}}, Ctx);
value(#token{}=Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

members(#token{name=comma}, _From, Ctx) ->
    {reply, ok, members, Ctx};
members(#token{name=objEnd}=Token, _From, #parser{stack=[Top|Stack]}=Ctx) ->
    occi_parser:send_event(Token, {reply, ok, Top, Ctx#parser{stack=Stack}}, Ctx);
members(#token{name=string, data=Val}, _From, Ctx) ->
    occi_parser:send_event(#token{name=key, data=list_to_binary(Val)}, {reply, ok, pair, Ctx}, Ctx);
members(#token{}=Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

elements(#token{name=comma}, _From, Ctx) ->
    {reply, ok, elements, Ctx};
elements(#token{name=arrEnd}=Token, _From, #parser{stack=[Top|Stack]}=Ctx) ->
    occi_parser:send_event(Token, {reply, ok, Top, Ctx#parser{stack=Stack}}, Ctx);
elements(#token{name=string, data=Val}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=list_to_binary(Val)}, {reply, ok, elements, Ctx}, Ctx);
elements(#token{name=float, data=Val}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=Val}, {reply, ok, elements, Ctx}, Ctx);
elements(#token{name=integer, data=Val}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=Val}, {reply, ok, elements, Ctx}, Ctx);
elements(#token{name=true}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=true}, {reply, ok, elements, Ctx}, Ctx);
elements(#token{name=false}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=false}, {reply, ok, elements, Ctx}, Ctx);
elements(#token{name=null}, _From, Ctx) ->
    occi_parser:send_event(#token{name=value, data=null}, {reply, ok, elements, Ctx}, Ctx);
elements(#token{name=arrBegin}=Token, _From, #parser{stack=Stack}=Ctx) ->
    occi_parser:send_event(Token, {reply, ok, array, Ctx#parser{stack=[elements|Stack]}}, Ctx);
elements(#token{name=objBegin}=Token, _From, #parser{stack=Stack}=Ctx) ->
    occi_parser:send_event(Token, {reply, ok, object, Ctx#parser{stack=[elements|Stack]}}, Ctx);
elements(#token{}=Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

eof(_E, _F, Ctx) ->
    {stop, eof, Ctx}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
