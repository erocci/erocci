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
-module(occi_parser_json).
-compile({parse_transform, lager_transform}).

-behaviour(gen_fsm).

-include("occi.hrl").
-include("occi_parser.hrl").

%% API
-export([start_parser/0,
	 reset_parser/1,
	 stop_parser/1,
	 parse/1,
	 parse/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% JSON parsing states
-export([init_json/3,
	 array/3,
	 object/3,
	 pair/3,
	 value/3]).

%% OCCI parsing states
-export([init_occi/3,
	 resources/3,
	 resources_list/3,
	 resource/3,
	 resource_kind/3,
	 resource_attributes/3,
	 resource_attribute/3,
	 resource_attribute_list/3]).

%% Shared states
-export([eof/3]).

-define(SERVER, ?MODULE).

-record(state, {stack          = []            :: [atom()],
		next           = undefined     :: pid(),
		manifest       = undefined     :: term(),
		resource       = undefined     :: term(),
		link           = undefined     :: term(),
		attrKey        = undefined     :: term()}).

-type(parser_id() :: reference()).
-type(parser_result() :: term()).

%%%===================================================================
%%% API
%%%===================================================================
parse(Data) ->
    JP = start_parser(),
    parse(JP, Data).

-spec parse(parser_id(), binary()) -> parser_result().
parse(Parser, Data) when is_list(Data)->
    case occi_scanner_json:string(Data) of
	{ok, Tokens, _EndLine} ->
	    send_events(Parser, Tokens);
	{error, Err, Line} ->
	    lager:error("Error parsing json data at line: ~p~n", [Line]),
	    throw({error, Err, Line})
    end;
parse(Parser, Data) when is_binary(Data) ->
    parse(Parser, binary_to_list(Data)).

start_parser() ->
    case gen_fsm:start(?MODULE, json, []) of
	{ok, Pid} -> Pid;
	Other -> 
	    lager:error("Error starting json parser: ~p~n", [Other])
    end.

reset_parser(Ref) ->
    gen_fsm:send_all_state_event(Ref, stop).

stop_parser(Ref) ->
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
init(occi) ->
    {ok, init_occi, #state{stack=[init_occi]}};
init(json) ->
    case gen_fsm:start(?MODULE, occi, []) of
	{ok, Pid} -> 
	    {ok, init_json, #state{next=Pid, stack=[init_json]}};
	Err ->
	    {stop, Err}
    end.

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
handle_event(reset, _, State) ->
    {next_state, init, State};
handle_event(stop, _, #state{next=undefined}=State) ->
    {stop, normal, State};
handle_event(stop, _, #state{next=Pid}=State) ->
    gen_fsm:send_all_state_event(Pid, stop),
    {stop, normal, State#state{next=undefined}};
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
handle_sync_event(eof, _From, _StateName, #state{next=undefined}=State) ->
    {stop, eof, State};
handle_sync_event(eof, _From, _StateName, #state{next=Pid}=State) ->
    gen_fsm:sync_send_all_state_event(Pid, eof),
    {stop, eof, State#state{next=undefined}};
handle_sync_event(Event, _From, _StateName, State) ->
    parse_error(Event, State).

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
terminate(Reason, StateName, #state{next=undefined}=State) ->
    lager:debug("Terminate with reason ~p in state ~s [~p]", [Reason, StateName, State]);
terminate(Reason, StateName, #state{next=Pid}=State) ->
    gen_fsm:send_all_state_event(Pid, stop),
    lager:debug("Terminate with reason ~p in state ~s [~p]", [Reason, StateName, State]).

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
init_json(#token{name=objBegin}, _From, State) ->
    push(object, State);
init_json(#token{name=arrBegin}, _From, State) ->
    push(array, State);
init_json(#token{}=Token, _From, State) ->
    parse_error(Token, State).

array(#token{name=arrEnd}=Token, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, Token, 
	       pop(State), State);
array(#token{name=string, data=Data}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=arrValue, data=Data}, 
	       {reply, ok, array, State}, State);
array(#token{name=float, data=Data}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=arrValue, data=Data},
	      {reply, ok, array, State}, State);
array(#token{name=integer, data=Data}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=arrValue, data=Data},
	       {reply, ok, array, State}, State);
array(#token{name=true}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=arrValue, data=true},
	       {reply, ok, array, State}, State);
array(#token{name=false}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=arrValue, data=false},
	       {reply, ok, array, State}, State);
array(#token{name=null}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=arrValue, data=null},
	       {reply, ok, array, State}, State);
array(#token{name=arrBegin}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=arrArr},
	       push(array, State), State);
array(#token{name=objBegin}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=arrObj},
	       push(object, State), State);
array(#token{}=Token, _From, State) ->
    parse_error(Token, State).

object(#token{name=objEnd}=Token, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, Token,
	       pop(State), State);
object(#token{name=string, data=Data}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=objKey, data=Data},
	       push(pair, State), State);
object(#token{}=Token, _From, State) ->
    parse_error(Token, State).

pair(#token{name=colon}, _From, State) ->
    push(value, State);
pair(#token{name=comma}, _From, State) ->
    {reply, ok, pair, State};
pair(#token{name=string, data=Data}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=objKey, data=Data},
	       {reply, ok, pair, State}, State);
pair(#token{name=objEnd}=Token, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, Token,
	       pop(State), State);
pair(#token{name=arrEnd}=Token, _From, #state{next=Pid}=State) ->
    send_event_next(Token, Pid,
	       pop(State), State);
pair(#token{}=Token, _From, State) ->
    parse_error(Token, State).

value(#token{name=string, data=Data}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=objValue, data=Data},
	       pop(State), State);
value(#token{name=float, data=Data}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=objValue, data=Data},
	       pop(State), State);
value(#token{name=integer, data=Data}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=objValue, data=Data}, 
	       pop(State), State);
value(#token{name=true}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=objValue, data=true},
	       pop(State), State);
value(#token{name=false}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=objValue, data=false},
	       pop(State), State);
value(#token{name=null}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=objValue, data=null},
	       pop(State), State);
value(#token{name=arrBegin}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=objArr},
	       push(array, State), State);
value(#token{name=objBegin}, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, #token{name=objObj},
	       push(object, State), State);
value(#token{name=arrEnd}=Token, _From, #state{next=Pid}=State) ->
    send_event_next(Pid, Token,
	       pop(State), State);
value(#token{}=Token, _From, State) ->
    parse_error(Token, State).

%% OCCI parsing states
init_occi(#token{name=objKey, data="resources"}, _From, State) ->
    push(resources, State#state{manifest=occi_manifest:new()});
init_occi(Token, _From, State) ->
    parse_error(Token, State).

resources(#token{name=objArr}, _From, State) ->
    push(resources_list, State);
resources(#token{name=objEnd}, _From, State) ->
    {reply, {eof, State#state.manifest}, eof, State#state{manifest=undefined}};
resources(Token, _From, State) ->
    parse_error(Token, State).

resources_list(#token{name=arrObj}, _From, State) ->
    push(resource, State#state{resource=occi_resource:new()});
resources_list(#token{name=arrEnd}, _From, State) ->
    pop(State);
resources_list(Token, _From, State) ->
    parse_error(Token, State).

resource(#token{name=objKey, data="kind"}, _From, State) ->
    push(resource_kind, State);
resource(#token{name=objKey, data="attributes"}, _From, State) ->
    push(resource_attributes, State);
resource(#token{name=objEnd}, _From, #state{resource=Res}=State) ->
    Manifest = occi_manifest:add_resource(State#state.manifest, Res),
    pop(State#state{manifest=Manifest, resource=undefined});
resource(Token, _From, State) ->
    parse_error(Token, State).

resource_kind(#token{name=objValue, data=Data}, _From, #state{resource=Res}=State) ->
    case build_category(kind, Data) of
	{ok, #occi_cid{}=Id} ->
	    pop(State#state{resource=occi_resource:set_cid(Res, Id)});
	{error, Err} ->
	    {reply, {error, Err}, eof, State}
    end;
resource_kind(Token, _From, State) ->
    parse_error(Token, State).

resource_attributes(#token{name=objObj}, _From, State) ->
    push(resource_attribute, State);
resource_attributes(Token, _From, State) ->
    parse_error(Token, State).

resource_attribute(#token{name=objKey, data=Data}, _From, State) ->
    {reply, ok, resource_attribute, State#state{attrKey=Data}};
resource_attribute(#token{name=objValue, data=Data}, _From, 
		   #state{attrKey=A, resource=Res}=State) ->
    {reply, ok, resource_attribute, 
     State#state{resource=occi_resource:set_attr_value(Res, A, Data)}};
resource_attribute(#token{name=objArr}, _From, State) ->
    push(resource_attribute_list, State);
resource_attribute(#token{name=objEnd}, _From, 
		   #state{stack=[_S0, _S1, S2|Stack]}=State) ->
    {reply, ok, S2, State#state{stack=[S2|Stack], attrKey=undefined}};
resource_attribute(Token, _From, State) ->
    parse_error(Token, State).

resource_attribute_list(#token{name=arrValue, data=Data}, _From, 
			#state{resource=Res, attrKey=A}=State) ->
    {reply, ok, resource_attribute_list, 
     State#state{resource=occi_resource:add_attr_value(Res, A, Data)}};
resource_attribute_list(#token{name=arrEnd}, _From, State) ->
    pop(State);
resource_attribute_list(Token, _From, State) ->
    parse_error(Token, State).    

%% Shared states
eof(_E, _F, State) ->
    {stop, eof, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_events(Parser, [Event|Tail]) ->
    lager:debug("Event: ~p~n", [Event]),
    case catch gen_fsm:sync_send_event(Parser, Event) of
	{'EXIT', Reason} ->
	    gen_fsm:send_all_state_event(Parser, stop),
	    throw({error, {'EXIT', Reason}});
	ok ->
	    send_events(Parser, Tail);
	{error, Reason} ->
	    gen_fsm:send_all_state_event(Parser, stop),
	    throw({error, {parse_error, Reason}});
	{eof, Result} ->
	    gen_fsm:send_all_state_event(Parser, stop),
	    Result
    end;
send_events(Parser, []) ->
    lager:debug("Event: eof~n", []),
    case catch gen_fsm:sync_send_all_state_event(Parser, eof) of
	{'EXIT', Reason} ->
	    throw({error, {'EXIT', Reason}});
	{error, Reason} ->
	    throw({error, {parse_error, Reason}})
    end,
    gen_fsm:send_all_state_event(Parser, stop).

send_event_next(Pid, Event, IfOk, State) ->
    case catch gen_fsm:sync_send_event(Pid, Event) of
	{'EXIT', Reason} ->
	    gen_fsm:send_all_state_event(Pid, stop),
	    {reply, {error, Reason}, eof, State#state{next=undefined}};
	ok ->
	    IfOk;
	{eof, Result} ->
	    gen_fsm:send_all_state_event(Pid, stop),
	    {reply, {eof, Result}, eof, State#state{next=undefined}};
	{error, Reason} ->
	    gen_fsm:send_all_state_event(Pid, stop),
	    {reply, {error, Reason}, eof, State#state{next=undefined}}
    end.

push(Next, #state{stack=undefined}=State) ->
    push(Next, State#state{stack=[]});
push(Next, #state{stack=Stack}=State) ->
    {reply, ok, Next, State#state{stack=[Next|Stack]}}.

pop(#state{stack=[]}=State) ->
    {stop, {error, stack_error}, State};
pop(#state{stack=[_Cur,Previous|Stack]}=State) ->
    {reply, ok, Previous, State#state{stack=[Previous|Stack]}}.

parse_error(#token{}=Token, State) ->
    Err = build_err(Token),
    lager:error(Err),
    log_stack(State#state.stack),
    {stop, {error, Err}, State}.

build_err(#token{name=Name, pos=undefined, data=undefined}) ->
    io_lib:format("Invalid term: ~p~n", [Name]);
build_err(#token{name=Name, pos=undefined, data=Data}) ->
    io_lib:format("Invalid term: ~p(~p)~n", [Name, Data]);
build_err(#token{name=Name, pos=Pos, data=undefined}) ->
    io_lib:format("Invalid term at line ~p: ~p~n", [Pos, Name]);
build_err(#token{name=Name, pos=Pos, data=Data}) ->
    io_lib:format("Invalid term at line ~p: ~p(~p)~n", [Pos, Name, Data]).

log_stack(Stack) ->
    lager:error("Parser states stack:~n", []),
    log_stack2(Stack).

log_stack2([]) ->
    ok;
log_stack2([Head|Tail]) ->
    lager:error("\t~s~n", [Head]),
    log_stack2(Tail).

build_category(Class, Str) ->
    case string:tokens(Str, "#") of
	[Scheme, Term] ->
	    {ok, #occi_cid{scheme=list_to_atom(Scheme++"#"), term=list_to_atom(Term), class=Class}};
	_ ->
	    {error, {invalid_category, Str}}
    end.
