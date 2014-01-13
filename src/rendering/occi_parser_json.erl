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
-export([start/0,
	 stop/1,
	 parse/2]).
-export([parse_resource/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% OCCI parsing states
-export([init/3,
	 request/3,
	 resources_req/3,
	 resources/3,
	 resource/3,
	 resource_kind/3,
	 resource_mixins/3,
	 resource_mixin/3,
	 resource_attributes/3,
	 resource_attribute/3,
	 resource_id/3,
	 resource_links/3,
	 resource_link/3,
	 eof/3]).

-define(SERVER, ?MODULE).

-record(state, {request        = #occi_request{}   :: occi_request(),
		resource       = undefined         :: term(),
		link           = undefined         :: term(),
		attrKey        = undefined         :: term()}).

%%%===================================================================
%%% API
%%%===================================================================
parse_resource(Data, #occi_kind{id=Cid}) ->
    P = start(),
    case parse(P, Data) of
	{error, Reason} ->
	    {error, Reason};
	{ok, #occi_request{resources=[#occi_resource{cid=Cid}=Res]}} ->
	    {ok, Res};
	Ret ->
	    lager:error("### parse_resource result: ~p~n", [Ret]),
	    {error, invalid_request}
    end.	    

-spec parse(parser(), binary()) -> parser_result().
parse(#parser{src=#parser{mod=Mod}=Src}, Data) ->
    Mod:parse(Src, Data).

start() ->
    Parser = #parser{mod=?MODULE, stack=[eof], state=#state{}},
    case gen_fsm:start(?MODULE, Parser, []) of
	{ok, Pid} -> 
	    Src = occi_parser_json0:start(Parser#parser{id=Pid}),
	    Parser#parser{id=Pid, src=Src};
	Err -> 
	    lager:error("Error starting occi/json parser: ~p~n", [Err]),
	    throw(Err)
    end.

stop(#parser{id=Ref, src=#parser{mod=Mod}=Src}) ->
    Mod:stop(Src),
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
init(#parser{}=Parser) ->
    {ok, init, Parser}.

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
handle_event(stop, _, State) ->
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
handle_sync_event(eof, _From, _StateName, State) ->
    {stop, eof, State};
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

%% OCCI parsing states
init(#token{name=objBegin}, _From, Ctx) ->
    {reply, ok, request, Ctx};
init(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

request(#token{name=key, data="resources"}, _From, Ctx) ->
    {reply, ok, resources_req, Ctx};
request(#token{name=key, data="links"}, _From, Ctx) ->
    % TODO
    {stop, {error, invalid_request}, Ctx};
request(#token{name=key, data="action"}, _From, Ctx) ->
    % TODO
    {stop, {error, invalid_request}, _From, Ctx};
request(#token{name=key, data="kinds"}, _From, Ctx) ->
    % TODO
    {stop, {error, invalid_request}, Ctx};
request(#token{name=key, data="mixins"}, _From, Ctx) ->
    % TODO
    {stop, {error, invalid_request}, Ctx};
request(#token{name=key, data="actions"}, _From, Ctx) ->
    % TODO
    {stop, {error, invalid_request}, Ctx};
request(#token{name=objEnd}, _From, #parser{state=#state{request=Req}}=Ctx) ->
    {reply, {eof, Req}, eof, Ctx};
request(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resources_req(#token{name=arrBegin}, _From, Ctx) ->
    {reply, ok, resources, Ctx};
resources_req(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resources(#token{name=objBegin}, _From, #parser{state=State}=Ctx) ->
    {reply, ok, resource, Ctx#parser{state=State#state{resource=occi_resource:new()}}};
resources(#token{name=arrEnd}, _From, Ctx) ->
    {reply, ok, request, Ctx};
resources(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).
    
resource(#token{name=key, data="kind"}, _From, Ctx) ->
    {reply, ok, resource_kind, Ctx};
resource(#token{name=key, data="mixins"}, _From, Ctx) ->
    {reply, ok, resource_mixins, Ctx};
resource(#token{name=key, data="attributes"}, _From, Ctx) ->
    {reply, ok, resource_attributes, Ctx};
resource(#token{name=key, data="id"}, _From, Ctx) ->
    {reply, ok, resource_id, Ctx};
resource(#token{name=key, data="links"}, _From, Ctx) ->
    {reply, ok, resource_links, Ctx};
resource(#token{name=objEnd}, _From, #parser{state=#state{resource=Res, request=Req}=State}=Ctx) ->
    {reply, ok, resources, 
     ?set_state(Ctx, State#state{resource=undefined, request=occi_request:add_resource(Req, Res)})};
resource(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_kind(#token{name=value, data=Val}, _From, #parser{state=#state{resource=Res}=State}=Ctx) ->
    case split_cid(Val) of
	{Scheme, Term} ->
	    Res2 = occi_resource:set_cid(Res, #occi_cid{class=kind, scheme=Scheme, term=Term}),
	    {reply, ok, resource, 
	     ?set_state(Ctx, State#state{resource=Res2})};
	parse_error ->
	    {reply, {error, invalid_cid}, eof, Ctx}
    end;
resource_kind(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_mixins(#token{name=arrBegin}, _From, Ctx) ->
    {reply, ok, resource_mixin, Ctx};
resource_mixins(#token{name=arrEnd}, _From, Ctx) ->
    {reply, ok, resource, Ctx};
resource_mixins(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_mixin(#token{name=value, data=Val}, _From, #parser{state=#state{resource=Res}=State}=Ctx) ->
    case split_cid(Val) of
	{Scheme, Term} ->
	    Res2 = occi_resource:add_mixin(Res, #occi_cid{class=mixin, scheme=Scheme, term=Term}),
	    {reply, ok, resource_mixins, 
	     ?set_state(Ctx, State#state{resource=Res2})};
	parse_error ->
	    {reply, {error, invalid_cid}, eof, Ctx}
    end;
resource_mixin(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_attributes(#token{name=objBegin}, _From, Ctx) ->
    occi_parser:push(resource_attribute, Ctx);
resource_attributes(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_attribute(#token{name=key, data=Val}, _From, #parser{state=State}=Ctx) ->
    {reply, ok, resource_attribute, ?set_state(Ctx, State#state{attrKey=Val})};
resource_attribute(#token{name=value, data=Val}, _From, 
		   #parser{state=#state{attrKey=A, resource=Res}=State}=Ctx) ->
    {reply, ok, resource_attribute, 
     ?set_state(Ctx, State#state{attrKey=undefined, resource=occi_resource:set_attr_value(Res, A, Val)})};
resource_attribute(#token{name=objEnd}, _From, Ctx) ->
    {reply, ok, resource, Ctx};
resource_attribute(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_id(#token{name=value, data=Val}, _From, #parser{state=#state{resource=Res}=State}=Ctx) ->
    {reply, ok, resource, 
     ?set_state(Ctx, State#state{resource=occi_resource:set_id(Res, Val)})};
resource_id(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_links(#token{name=arrBegin}, _From, Ctx) ->
    {reply, ok, resource_link, Ctx};
resource_links(#token{name=arrEnd}, _From, Ctx) ->
    {reply, ok, resource, Ctx};
resource_links(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).

resource_link(#token{name=value, data=Val}, _From, #parser{state=#state{resource=Res}=State}=Ctx) ->
    {reply, ok, resource_links, 
     ?set_state(Ctx, State#state{resource=occi_resource:add_link(Res, Val)})};
resource_link(Token, _From, Ctx) ->
    occi_parser:parse_error(Token, Ctx).
    
eof(_E, _F, Ctx) ->
    {stop, eof, Ctx}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
split_cid(Str) ->
    case string:tokens(Str, "#") of
	[Scheme, Term] ->
	    {list_to_atom(Scheme++"#"), list_to_atom(Term)};
	_ ->
	    parse_error
    end.		
