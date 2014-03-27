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
%%% @doc
%%%
%%% @end
%%% Created : 20 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_xmpp_client).
-compile({parse_transform, lager_transform}).

-behaviour(gen_server).

-include("occi.hrl").
-include("occi_xml.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

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

-record(state, {session, jid}).

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
	?occi_ns ->
	    handle_occi(Raw, State);
	?ns_roster ->
	    handle_roster(From, Raw, State);
	?ns_disco_info ->
	    handle_disco(From, Raw, State);
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
    S = #state{jid=Jid},
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

handle_roster(From, Raw, State) ->
    lager:debug("### ROSTER IQ from ~p: ~p~n", [From, Raw]),
    {noreply, State}.

handle_disco(From, Raw, #state{session=Session}=State) ->
    case exmpp_iq:get_type(Raw) of
	'get' ->
	    lager:debug("Sending disco#info result to ~p~n", [From]),
	    Res = exmpp_xml:append_children(
	    	    exmpp_iq:result(Raw), get_disco_info()),
	    exmpp_session:send_packet(Session, Res),
	    {noreply, State};
	'set' ->
	    lager:debug("Unexpected set IQ: ~p~n", [?ns_disco_info]),
	    {noreply, State};
	'result' ->
	    lager:debug("Unexpected set IQ: ~p~n", [?ns_disco_info]),
	    {noreply, State};
	'error' ->
	    lager:debug("Unexpected set IQ: ~p~n", [?ns_disco_info]),
	    {noreply, State}
    end.

handle_occi(Raw, #state{session=Session}=State) ->
    case exmpp_iq:get_type(Raw) of
	'get' ->
	    case occi_iq:get_node(Raw) of
		{error, _Err} ->
		    lager:error("Invalid OCCI IQ node: ~p~n", [_Err]),
		    Iq = exmpp_iq:error(Raw, 'bad-request'),
		    exmpp_session:send_packet(Session, Iq);
		#occi_node{}=Node ->
		    case occi_store:find(Node) of
			{ok, [Res]} ->
			    case occi_store:load(Res) of
				{ok, Res2} ->
				    Iq = occi_iq:result(Raw, Res2),
				    exmpp_session:send_packet(Session, Iq);
				{error, Err} ->
				    lager:error("Internal error: ~p~n", [Err]),
				    Iq = exmpp_iq:error(Raw, 'service-unavailable'),
				    exmpp_session:send_packet(Session, Iq)
			    end;
			{ok, _Res} ->
			    lager:error("OCCI IQ node: multiple answer: ~p~n", [_Res]),
			    Iq = exmpp_iq:error(Raw, 'bad-request'),
			    exmpp_session:send_packet(Session, Iq);
			{error, _Err} ->
			    lager:error("Internal error: ~p~n", [_Err]),
			    Iq = exmpp_iq:error(Raw, 'service-unavailable'),
			    exmpp_session:send_packet(Session, Iq)
		    end
	    end;
	_ ->
	    % TODO
	    ok
    end,
    {noreply, State}.

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
