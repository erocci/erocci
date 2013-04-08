%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_xmpp).

-behaviour(gen_fsm).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%% API
-export([start_link/0, stop/0, get_status/0, set_auth/2, connect/0]).

%% gen_server callbacks
-export([init/1, 
	 code_change/4,
	 handle_info/3,
	 handle_event/3,
	 handle_sync_event/4,
	 terminate/3]).

%% States
-export([wait_for_auth/2,
	 connected/2,
	 disconnected/2]).

-define(DEFAULT_PORT, 5222).
-define(SERVER, ?MODULE).

-record(state, {session                              :: pid(), 
		port                                 :: pos_integer(),
		auth_info = { undefined, undefined } :: { string(), string() }}).

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
    case gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []) 
    of
	{ok, Pid} -> {ok, Pid};
	{error, Reason} -> erlang:error({error, Reason})
    end.

stop() ->
    catch gen_fsm:sync_send_all_state_event(?MODULE, stop),
    ok.

connect() ->
    gen_fsm:send_event(?MODULE, connect).

set_auth(Jid, Passwd) ->
    gen_fsm:send_event(?MODULE, {set_auth, Jid, Passwd}).

get_status() ->
    Res = gen_fsm:sync_send_all_state_event(?MODULE, status),
    Res.

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
    Port = occi_config:get('xmpp.port', fun(V) when is_integer(V) -> V end,
			   ?DEFAULT_PORT),
    BaseJid = occi_config:get('xmpp.jid', fun(V) when is_binary(V) -> V end,
			      undefined),
    Passwd = occi_config:get('xmpp.password', fun(V) when is_binary(V) -> V end,
			     undefined),
    Session = exmpp_session:start(),
    State = #state{ port=Port, 
		    auth_info={ BaseJid, Passwd },
		    session=Session},
    setup(State).

setup(#state{ auth_info={ undefined, undefined } } = State) ->
    {ok, wait_for_auth, State};
setup(#state{ session=Session,
	      auth_info={ BaseJid, Passwd },
	      port=Port } = State) ->
    case do_connect(Session, BaseJid, Passwd, Port)
    of
	ok -> {ok, connected, State};
	nok -> {ok, disconnected, State}
    end.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_info(#received_packet{packet_type=message,
			     raw_packet=Packet} = Record,
	    StateName,
	    #state{session=Session} = State) ->
    echo_packet(Session, Packet),
    {next_state, StateName, State};
handle_info(_Record, StateName, State) ->
    {next_state, StateName, State}.

terminate(Reason, _StateName, #state{ session=Session }) ->
    io:format("### Terminating occi_xmpp: ~p~n", [Reason]),
    exmpp_session:stop(Session),
    ok.

handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(stop, _From, _StateName, State) ->
    Reply = ok,
    {stop, normal, Reply, State};
handle_sync_event(status, _From, connected, #state{ auth_info={ Jid, _ } } = State) ->
    Reply = { connected, Jid },
    {reply, Reply, connected, State};		
handle_sync_event(status, _From, disconnected, #state{ auth_info={ Jid, _ } } = State) ->
    Reply = { disconnected, Jid },
    {reply, Reply, disconnected, State};		
handle_sync_event(status, _From, wait_for_auth, State) ->
    Reply = { wait_for_auth, [] },
    {reply, Reply, wait_for_auth, State};		
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%% 
%% States
%%
wait_for_auth({set_auth, Jid, Passwd}, State) ->
    occi_config:set('occi.jid', Jid),
    occi_config:set('occi.passwd', Passwd),
    NewState = State#state{ auth_info={Jid, Passwd} },
    {next_state, disconnected, NewState};
wait_for_auth(_Event, State) ->
    {next_state, wait_for_auth, State}.

connected({set_auth, Jid, Passwd}, 
	  #state{ session=Session } = State) ->
    occi_config:set('occi.jid', Jid),
    occi_config:set('occi.passwd', Passwd),
    NewState = State#state{ auth_info={Jid, Passwd} },
    exmpp_session:stop(Session),
    {next_state, disconnected, NewState};
connected(_Event, State) ->
    {next_state, connected, State}.

disconnected(connect, 
	     #state{ session=Session, auth_info={ Jid, Passwd }, port=Port } = State) ->
    case do_connect(Session, Jid, Passwd, Port)
    of
	ok -> {next_state, connected, State};
	nok -> {next_state, disconnected, State}
    end;		
disconnected(_Event, State) ->
    {next_state, disconnected, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
do_connect(Session, BaseJid, Passwd, Port) when is_binary(BaseJid) ->
    do_connect(Session, binary_to_list(BaseJid), Passwd, Port);
do_connect(Session, BaseJid, Passwd, Port) when is_binary(Passwd) ->
    do_connect(Session, BaseJid, binary_to_list(Passwd), Port);
do_connect(Session, BaseJid, Passwd, Port) ->
    [User, Server] = string:tokens(BaseJid, "@"),
						% Create XMPP ID (Session Key)
    Jid = exmpp_jid:make(User, Server, random),
    exmpp_session:auth_info(Session, Jid, Passwd),
    {ok, _StreamID} = exmpp_session:connect_TCP(Session, Server, Port),
    try exmpp_session:login(Session, password)
    catch
	throw:{auth_error, 'not-authorized'} ->
	    nok
    end,
						% Send presence
    Status = exmpp_presence:set_status(exmpp_presence:available(), 
				       "OCCI Ready"),
    exmpp_session:send_packet(Session, Status),
    ok.
