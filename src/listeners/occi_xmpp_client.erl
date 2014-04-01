%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
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
%%% @doc List of callbacks for OCCI REST handler:
%%% Except specified, return {ok, xmlel(), any()} | 
%%%                          {error, xmlel() | atom(), any()} |
%%%                          {halt, xmlel(), any()}
%%%
%%% Mandatory:
%%%   init/2 -> {ok, xmlel(), any()}.
%%%
%%% Optionals:
%%%   service_available/2
%%%   known_methods/2
%%%   allowed_method/2
%%%   is_authorized/2
%%%   forbidden/2
%%%   resource_exists/2
%%%   delete_resource/2
%%%   is_conflict/2
%%%   accept_resource/2
%%%   update_resource/2
%%%   get_resource/2
%%%   terminate/2
%%%
%%% @end
%%% Created : 20 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_xmpp_client).
-compile({parse_transform, lager_transform}).

-behaviour(gen_server).

-include("occi.hrl").
-include("occi_xml.hrl").
-include("occi_xmpp.hrl").
-include_lib("erim/include/exmpp.hrl").
-include_lib("erim/include/exmpp_client.hrl").

%% occi_listener callbacks
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(ns_roster, 'jabber:iq:roster').
-define(ns_caps, 'http://jabber.org/protocol/caps').
-define(ns_disco_info, 'http://jabber.org/protocol/disco#info').
-define(ns_disco_items, 'http://jabber.org/protocol/disco#items').

-define(TIMEOUT, 5000).
-define(HANDLER, occi_xmpp_rest).

-record(state, {session, 
		jid,
		handler,
		handler_state,
		exists   = false}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Props) ->
    lager:info("Starting XMPP listener: ~p~n", [proplists:get_value(jid, Props)]),
    application:start(exmpp),
    gen_server:start_link({local, Ref}, ?MODULE, validate_cfg(Props), []).

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
init(Props) ->
    Jid = proplists:get_value(jid, Props),
    Name = #uri{scheme='xmpp+occi', 
		host=exmpp_jid:domain(Jid), 
		userinfo=exmpp_jid:node(Jid)},
    occi_config:set(name, Name),
    session(Props).

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
handle_call(Req, _From, State) ->
    lager:debug("### <xmpp> request: ~p~n", [Req]),
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(Msg, State) ->
    lager:debug("### <xmpp> cast: ~p~n", [Msg]),
    {noreply, State}.

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
handle_info(#received_packet{packet_type=presence, from=From, type_attr=Type}, 
	    #state{session=Session, jid=MyJID}=State) ->
    JID = exmpp_jid:make(From),
    case Type of
	"subscribe" ->
	    % TODO: do not always accept subscription
	    presence_subscribed(Session, JID),
	    presence_subscribe(Session, JID),
	    {noreply, State};
	"subscribed" ->
	    % TODO: do not always accept subscription
	    presence_subscribed(Session, JID),
	    presence_subscribe(Session, JID),
	    {noreply, State};
	"available" ->
	    lager:debug("Received presence available from ~p~n", [JID]),
	    Iq = exmpp_client_disco:info(JID),
	    exmpp_session:send_packet(Session, exmpp_stanza:set_sender(Iq, MyJID)),
	    {noreply, State};
	Other ->
	    lager:debug("Received presence stanza from ~p: ~p~n", [JID, Other]),
	    {noreply, State}
    end;

handle_info(#received_packet{packet_type=iq, from=From, raw_packet=Raw}, #state{session=Session}=State) ->
    try exmpp_iq:get_payload_ns_as_atom(Raw) of
	?ns_occi_xmpp ->
	    handle_occi(Raw, State);
	?ns_roster ->
	    handle_roster(From, Raw, State),
	    {noreply, State};
	?ns_disco_info ->
	    handle_disco(From, Raw, State),
	    {noreply, State};
	'jabber:client' ->
	    {noreply, State};
	_Other ->
	    lager:debug("Unmanaged IQ from ~p: ~p~n", [From, Raw]),
	    Iq = exmpp_iq:error(Raw, 'feature-not-implemented'),
	    exmpp_session:send_packet(Session, Iq),
	    {noreply, State}
    catch throw:_Err ->
	    lager:debug("Malformed IQ from ~s~n", [exmpp_jid:to_binary(From)]),
	    Iq = exmpp_iq:error(Raw, 'bad-request'),
	    exmpp_session:send_packet(Session, Iq),
	    {noreply, State}
    end;

handle_info(Info, State) ->
    lager:debug("### <xmpp> receive: ~p~n", [Info]),
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
terminate(_Reason, #state{session=Session}) ->
    exmpp_session:stop(Session),
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
validate_cfg(Props) ->
    Str = case proplists:get_value(jid, Props) of
	      undefined -> throw({undefined_opt, jid});
	      V -> V
	  end,
    {User, Domain} = case string:tokens(Str, "@") of
			 [U, D] -> {U, D};
			 _ -> throw({error, {invalid_jid, Str}})
		     end,
    P2 = [{jid, exmpp_jid:make(User, Domain, <<"erocci">>)} | Props],
    P3 = case proplists:is_defined(server, Props) of
	     false -> [{server, Domain} | P2];
	     true -> P2
	 end,
    case proplists:get_value(passwd, P3) of
	undefined -> throw({undefined_opt, passwd});
	_S -> P3
    end.

session(Props) ->
    Session = exmpp_session:start(),
    Jid = proplists:get_value(jid, Props),
    Passwd = proplists:get_value(passwd, Props),
    exmpp_session:auth_basic_digest(Session, Jid, Passwd),
    S = #state{jid=Jid, handler=?HANDLER},
    case exmpp_session:connect_TCP(Session, proplists:get_value(server, Props)) of
	{ok, _SId} -> 
	    auth(S#state{session=Session});
	{ok, _SId, _F} ->
	    auth(S#state{session=Session});
	Other -> {stop, {session_error, Other}}
    end.

auth(#state{session=Session}=S) ->
    try exmpp_session:login(Session)
    catch throw:Err -> {stop, Err}
    end,
    Status = get_initial_presence("erocci ready"),
    exmpp_session:send_packet(Session, Status),
    {ok, S}.

presence_subscribed(Session, Recipient) ->
    Presence_Subscribed = exmpp_presence:subscribed(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribed, Recipient),
    exmpp_session:send_packet(Session, Presence).

presence_subscribe(Session, Recipient) ->
    Presence_Subscribe = exmpp_presence:subscribe(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribe, Recipient),
    exmpp_session:send_packet(Session, Presence).

handle_roster(From, Raw, _State) ->
    lager:debug("### ROSTER IQ from ~p: ~p~n", [From, Raw]).

handle_disco(From, Raw, #state{session=Session}) ->
    case exmpp_iq:get_type(Raw) of
	'get' ->
	    lager:debug("Sending disco#info result to ~p~n", [From]),
	    Res = exmpp_xml:append_children(
	    	    exmpp_iq:result(Raw), get_disco_info()),
	    exmpp_session:send_packet(Session, Res);
	'set' ->
	    lager:debug("Unexpected set IQ: ~p~n", [?ns_disco_info]);
	'result' ->
	    lager:debug("Unexpected set IQ: ~p~n", [?ns_disco_info]);
	'error' ->
	    lager:debug("Unexpected set IQ: ~p~n", [?ns_disco_info])
    end.

get_disco_info() ->
    Id = exmpp_xml:element(?ns_disco_info, identity,
			   [exmpp_xml:attribute(<<"category">>, <<"client">>),
			    exmpp_xml:attribute(<<"type">>, <<"occi">>),
			    exmpp_xml:attribute(<<"name">>, ?XMPP_CLIENT_ID)], 
			   []),
    Features = [exmpp_xml:element(?ns_disco_info, feature, 
				  [exmpp_xml:attribute(<<"var">>, ?occi_ns)],
				  [])],
    [Id | Features].

get_initial_presence(Str) ->
    Pkt = exmpp_presence:set_status(exmpp_presence:available(), Str),
    exmpp_xml:append_child(Pkt, exmpp_xml:element(?ns_caps, c,
						  [exmpp_xml:attribute(<<"hash">>, <<"sha-1">>),
						   exmpp_xml:attribute(<<"node">>, ?XMPP_NODE_ID),
						   exmpp_xml:attribute(<<"ver">>, get_caps_version())],
						  [])).
get_caps_version() ->
    base64:encode(crypto:hash(sha, atom_to_list(?occi_ns))).

%%%
%%% REST like state machine for OCCI IQ
%%%
handle_occi(Req, #state{handler=H}=S) ->
    try occi_iq:to_record(Req) of
	Req2 -> 
	    try H:init(Req2, S) of
		{ok, Req3, HandlerState} ->
		    service_available(Req3, S#state{handler_state=HandlerState})
	    catch Class:Reason ->
		    erlang:Class([
			  {reason, Reason},
			  {mfa, {H, init, 2}},
			  {stacktrace, erlang:get_stacktrace()},
			  {req, exmpp_xml:document_to_list(Req#occi_iq.raw)}
			 ])
	    end
    catch throw:_ -> respond(Req, S, 'bad-request')
    end.

service_available(Req, State) ->
    expect(Req, State, service_available, true, fun known_methods/2, 'service-unavailable').

known_methods(#occi_iq{op=Op}=Req, State) ->
    case call(Req, State, known_methods) of
	no_call when Op =:= 'get'; Op =:= save; Op =:= update; Op =:= delete ->
	    next(Req, State, fun allowed_methods/2);
	no_call ->
	    next(Req, State, 'feature-not-implemented');
	{halt, Req2, HandlerState} ->
	    rest_terminate(Req2, State#state{handler_state=HandlerState});
	{List, Req2, HandlerState} ->
	    State2 = State#state{handler_state=HandlerState},
	    case lists:member(Op, List) of
		true -> next(Req2, State2, fun allowed_methods/2);
		false -> next(Req, State2, 'feature-not-implemented')
	    end
    end.

allowed_methods(#occi_iq{op=Op}=Req, State) ->
    case call(Req, State, allowed_methods) of
	no_call when Op =:= 'get'; Op =:= save; Op =:= update; Op =:= delete ->
	    next(Req, State, fun is_authorized/2);
	no_call ->
	    next(Req, State, 'not-allowed');
	{halt, Req2, HandlerState} ->
	    rest_terminate(Req2, State#state{handler_state=HandlerState});
	{List, Req2, HandlerState} ->
	    State2 = State#state{handler_state=HandlerState},
	    case lists:member(Op, List) of
		true ->
		    next(Req2, State2, fun is_authorized/2);
		false ->
		    next(Req, State, 'not-allowed')
	    end
    end.

%% is_authorized/2 should return true or false.
is_authorized(Req, State) ->
    case call(Req, State, is_authorized) of
	no_call ->
	    forbidden(Req, State);
	{halt, Req2, HandlerState} ->
	    rest_terminate(Req2, State#state{handler_state=HandlerState});
	{true, Req2, HandlerState} ->
	    forbidden(Req2, State#state{handler_state=HandlerState});
	{false, Req2, HandlerState} ->
	    respond(Req2, State#state{handler_state=HandlerState}, 'not-authorized')
    end.

forbidden(Req, State) ->
    expect(Req, State, forbidden, false, fun resource_exists/2, 'forbidden').

resource_exists(#occi_iq{op=save}=Req, State) ->
    case call(Req, State, resource_exists) of
	no_call ->
	    next(Req, State, fun method/2);
	{halt, Req2, HandlerState} ->
	    rest_terminate(Req2, State#state{handler_state=HandlerState});
	{true, Req2, HandlerState} ->
	    next(Req2, State#state{exists=true, handler_state=HandlerState}, fun method/2);
	{false, Req2, HandlerState} ->
	    next(Req2, State#state{handler_state=HandlerState}, fun method/2)
    end;

resource_exists(Req, State) ->
    expect(Req, State, resource_exists, true, fun method/2, 'item-not-found').

method(#occi_iq{op=delete}=Req, State) ->
    delete_resource(Req, State);
method(#occi_iq{op=save}=Req, State) ->
    is_conflict(Req, State);
method(#occi_iq{op=update}=Req, State) ->
    update_resource(Req, State);
method(#occi_iq{op='get'}=Req, State) ->
    set_resp_body(Req, State).

%% delete_resource/2 should start deleting the resource and return.
delete_resource(Req, State) ->
    expect(Req, State, delete_resource, false, 500, fun delete_completed/2).

%% delete_completed/2 indicates whether the resource has been deleted yet.
delete_completed(Req, State) ->
    next(Req, State, fun respond/2).

is_conflict(Req, State) ->
    expect(Req, State, is_conflict, false, fun accept_resource/2, 'conflict').

accept_resource(Req, State) ->
    try 
	case call(Req, State, accept_resource) of
	    {halt, Req2, HandlerState2} ->
		rest_terminate(Req2, State#state{handler_state=HandlerState2});
	    {true, Req2, HandlerState2} ->
		State2 = State#state{handler_state=HandlerState2},
		next(Req2, State2, fun respond/2);
	    {false, Req2, HandlerState2} ->
		State2 = State#state{handler_state=HandlerState2},
		respond(Req2, State2, 'not-acceptable')
	end 
    catch Class:Reason = {case_clause, no_call} ->
	    error_terminate(Req, State, Class, Reason, accept_resource)
    end.

update_resource(Req, State) ->
    try 
	case call(Req, State, update_resource) of
	    {halt, Req2, HandlerState2} ->
		rest_terminate(Req2, State#state{handler_state=HandlerState2});
	    {true, Req2, HandlerState2} ->
		State2 = State#state{handler_state=HandlerState2},
		next(Req2, State2, fun respond/2);
	    {false, Req2, HandlerState2} ->
		State2 = State#state{handler_state=HandlerState2},
		respond(Req2, State2, 'not-acceptable')
	end 
    catch Class:Reason = {case_clause, no_call} ->
	    error_terminate(Req, State, Class, Reason, update_resource)
    end.

set_resp_body(Req, State) ->
    try
	case call(Req, State, get_resource) of
	    {halt, Req2, HandlerState2} ->
		rest_terminate(Req2, State#state{handler_state=HandlerState2});
	    {Body, Req2, HandlerState2} ->
		State2 = State#state{handler_state=HandlerState2},
		Req3 = occi_iq:result(Req2, Body),
		respond(Req3, State2)
	end 
    catch Class:Reason = {case_clause, no_call} ->
	    error_terminate(Req, State, Class, Reason, get_resource)
    end.

%%% REST primitives
expect(Req, State, Callback, Expected, OnTrue, OnFalse) ->
    case call(Req, State, Callback) of
	no_call ->
	    next(Req, State, OnTrue);
	{halt, Req2, HandlerState} ->
	    State2 = State#state{handler_state=HandlerState},
	    case occi_iq:is_error(Req) of
		true ->
		    respond(Req2, State2);
		false ->
		    Iq = occi_iq:error(Req, 'service-not-available'),
		    respond(Iq, State2)
	    end;
	{Expected, Req2, HandlerState} ->
	    next(Req2, State#state{handler_state=HandlerState}, OnTrue);
	{_Unexpected, Req2, HandlerState} ->
	    next(Req2, State#state{handler_state=HandlerState}, OnFalse)
    end.

call(Req, State=#state{handler=Handler, handler_state=HandlerState}, Callback) ->
    case erlang:function_exported(Handler, Callback, 2) of
	true ->
	    try
		Handler:Callback(Req, HandlerState)
	    catch Class:Reason ->
		    error_terminate(Req, State, Class, Reason, Callback)
	    end;
	false ->
	    no_call
    end.

next(Req, State, Next) when is_function(Next) ->
    Next(Req, State);
next(Req, State, StatusCode) when is_atom(StatusCode) ->
    respond(Req, State, StatusCode).

respond(#occi_iq{raw=Raw}=Req, #state{session=Session}=State) ->
    case exmpp_iq:get_type(Raw) of
	result ->
	    exmpp_session:send_packet(Session, Raw);
	error ->
	    exmpp_session:send_packet(Session, Raw);
	_ ->
	    Iq = occi_iq:result(Req),
	    exmpp_session:send_packet(Session, Iq#occi_iq.raw)
    end,
    rest_terminate(Req, State).

respond(#occi_iq{}=Req, #state{session=Session}=State, Code) ->
    Iq = occi_iq:error(Req, Code),
    exmpp_session:send_packet(Session, Iq#occi_iq.raw),
    rest_terminate(Req, State).

error_terminate(#occi_iq{}=Req, #state{session=Session, handler=Handler, handler_state=HandlerState},
		Class, Reason, Callback) ->
    Iq = occi_iq:error(Req, 'undefined-condition'),
    exmpp_session:send_packet(Session, Iq#occi_iq.raw),
    erlang:Class([
		  {reason, Reason},
		  {mfa, {Handler, Callback, 2}},
		  {stacktrace, erlang:get_stacktrace()},
		  {req, exmpp_xml:document_to_list(Req#occi_iq.raw)},
		  {state, HandlerState}
		 ]).

rest_terminate(Req, #state{handler=Handler, handler_state=HandlerState}=State) ->
    case erlang:function_exported(Handler, terminate, 2) of
	true -> ok = Handler:terminate(Req, HandlerState);
	false -> ok
    end,
    {noreply, State}.
