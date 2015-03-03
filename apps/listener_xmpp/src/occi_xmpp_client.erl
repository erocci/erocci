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

-include("occi_xmpp.hrl").
-include("occi.hrl").
-include("occi_log.hrl").
-include_lib("erim/include/erim.hrl").
-include_lib("erim/include/erim_client.hrl").

%% occi_listener callbacks
-export([start_link/2]).

%% erim_client callbacks
-export([init/2,
	 initial_presence/1,
	 approve/2,
	 approved/2]).
-export([msg_chat/2]).

-record(state, {ref    :: pid()}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Ref, Props) ->
    ?info("Starting XMPP listener: ~p~n", [proplists:get_value(jid, Props)]),
    application:ensure_all_started(erim),
    application:ensure_all_started(occi_authnz),
    Handlers = [{?ns_occi_xmpp, occi_xmpp_handler, []}],
    Opts = build_opts([{handlers, Handlers},
		       {node, ?XMPP_CLIENT_ID}
		       | Props]),
    erim_client:start_link(Ref, ?MODULE, Opts).

%%%===================================================================
%%% erim_client callbacks
%%%===================================================================
init(_Opts, Ref) ->
    {ok, #state{ref=Ref}}.

initial_presence(S) ->
    {#erim_presence{status= <<"OCCI ready !">>, priority=1}, S}.

approve(#received_packet{}=_Pkt, S) ->
    {both, S}.

approved(#received_packet{}=_Pkt, S) ->
    {ok, S}.

% echo, just for fun
msg_chat(#received_packet{raw_packet=Raw}, #state{ref=Ref}=S) ->
    From = exmpp_xml:get_attribute(Raw, <<"from">>, <<"unknown">>),
    To = exmpp_xml:get_attribute(Raw, <<"to">>, <<"unknown">>),
    P2 = exmpp_xml:set_attribute(Raw, <<"from">>, To),
    P3 = exmpp_xml:set_attribute(P2, <<"to">>, From),
    Echo = exmpp_xml:remove_attribute(P3, <<"id">>),
    erim_client:send(Ref, Echo),
    {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
build_opts(Opts) ->
    Creds = build_credentials(Opts),
    build_server([{creds, Creds} | Opts]).

build_credentials(Opts) ->
    Str = case proplists:get_value(jid, Opts) of
	      undefined -> throw({undefined_opt, jid});
	      V -> V
	  end,
    case string:tokens(Str, "@") of
		[U, "local"] ->
		    {local, exmpp_jid:make(U, "local", ?XMPP_RESOURCE)};
		[U, D] ->
		    Jid = exmpp_jid:make(U, D, ?XMPP_RESOURCE),
		    case proplists:get_value(passwd, Opts) of
			undefined -> throw({undefined_opt, passwd});
			S when is_binary(S) -> 
			    {Jid, S};
			S when is_list(S) ->
			    {Jid, list_to_binary(S)};
			_Else ->
			    throw({error, {invalid_password, _Else}})
		    end;
		_ ->
		    throw({error, {invalid_jid, Str}})
    end.

build_server(Opts) ->
    case proplists:get_value(server, Opts) of
	undefined ->
	    case proplists:get_value(creds, Opts) of
		{local, _} -> Opts;
		{#jid{domain=D}, _} -> [{server, binary_to_list(D)} | Opts]
	    end;
	Server ->
	    [{server, Server} | Opts]
    end.
