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

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%% occi_listener callbacks
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(TIMEOUT, 5000).

-record(state, {session}).

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
    case session(Props) of
	{ok, Session} ->
	    case auth(Session) of
		{ok, Session} ->
		    {ok, #state{session=Session}};
		{error, Err} ->
		    {stop, Err}
	    end;
	{error, Err} ->
	    {stop, Err}
    end.

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
validate_cfg(Props) ->
    Str = case proplists:get_value(jid, Props) of
	      undefined -> throw({undefined_opt, jid});
	      V -> V
	  end,
    {User, Domain} = case string:tokens(Str, "@") of
			 [U, D] -> {U, D};
			 _ -> throw({error, {invalid_jid, Str}})
		     end,
    P2 = [{jid, exmpp_jid:make(User, Domain, random)} | Props],
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
    exmpp_session:auth_basic_digest(Session,
				    proplists:get_value(jid, Props),
				    proplists:get_value(passwd, Props)),
    case exmpp_session:connect_TCP(Session, proplists:get_value(server, Props)) of
	{ok, _SId} -> 
	    {ok, Session};
	{ok, _SId, _F} ->
	    {ok, Session};
	Other -> {error, Other}
    end.

auth(Session) ->
    try exmpp_session:login(Session)
    catch throw:Err -> {error, Err}
    end,
    Status = exmpp_presence:set_status(
	       exmpp_presence:available(), "erocci ready"),
    exmpp_session:send_packet(Session, Status),
    {ok, Session}.
