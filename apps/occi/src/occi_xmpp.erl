%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_xmpp).

-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
				 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {session, jid}).

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
start_link() ->
		gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
		Session = exmpp_session:start(),
		BaseJid = occi_config:get_env("OCCI_XMPP_JID"),
		Passwd = occi_config:get_env("OCCI_XMPP_PASSWD"),
		[User, Server] = string:tokens(BaseJid, "@"),
		% Create XMPP ID (Session Key)
		Jid = exmpp_jid:make(User, Server, random),
		exmpp_session:auth_basic_digest(Session, Jid, Passwd),
		{ok, _StreamID} = exmpp_session:connect_TCP(Session, Server, 5222),
		login(Session, Jid, Passwd),
		{ok, #state{session=Session, jid=Jid}}.

 
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
handle_call(_Msg, _From, State) ->
		{reply, ok, State}.


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
handle_cast(stop, State) ->
		{stop, normal, State};
handle_cast(_Msg, State) ->
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
handle_info(#received_packet{packet_type=message,
														 raw_packet=Packet}, 
						#state{session=Session} = State) ->
		echo_packet(Session, Packet),
		{noreply, State};
handle_info(_Record, State) ->
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
%% Send the same packet back for each message received
echo_packet(Session, Packet) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, <<"from">>, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, <<"to">>, From),
    NewPacket = exmpp_xml:remove_attribute(TmpPacket2, <<"id">>),
    exmpp_session:send_packet(Session, NewPacket).

login(Session, _Jid, Passwd) ->
		try exmpp_session:login(Session)
		catch
				throw:{auth_error, 'not-authorized'} ->
						%% Try creating a new user:
						io:format("Register~n",[]),
						%% In a real life client, we should trap error case here
						%% and print the correct message.
						exmpp_session:register_account(Session, Passwd),
						%% After registration, retry to login:
						exmpp_session:login(Session)
    end,
    % Send presence
		Status = exmpp_presence:set_status(exmpp_presence:available(), 
																			 "OCCI Ready"),
		exmpp_session:send_packet(Session, Status),
		Session.
