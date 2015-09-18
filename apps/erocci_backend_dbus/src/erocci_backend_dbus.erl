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
%%% Created :  31 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(erocci_backend_dbus).

-behaviour(occi_backend).

-include("occi.hrl").
-include("occi_log.hrl").
-include_lib("dbus/include/dbus_client.hrl").

-define(BACKEND_IFACE, <<"org.ow2.erocci.backend">>).

%% occi_backend callbacks
-export([init/1,
		 terminate/1]).
-export([update/2,
		 save/2,
		 delete/2,
		 find/2,
		 load/3,
		 action/3]).

-record(state, {conn      :: dbus_connection(),
				backend   :: dbus_proxy()}).

%%%===================================================================
%%% occi_backend callbacks
%%%===================================================================
init(#occi_backend{opts=Props}) ->
	try parse_opts(Props) of
		{Service, Opts} ->
			case connect_backend(Service) of
				{ok, Bus, Backend} ->
					?info("Initializing backend service: ~s~n", [Service]),
					case dbus_proxy:call(Backend, ?BACKEND_IFACE, <<"init">>, [Opts]) of
						{ok, Schemas} ->
							{ok, process_schemas(Schemas, []), #state{conn=Bus, backend=Backend}};
						{error, {Code, Err}} ->
							?debug("Error initializing: ~n"
								   "Code=~s~n"
								   "Reason=~n~s~n", [Code, Err]),
							{error, Err}
					end;
				{error, Err} ->
					{error, Err}
			end
    catch throw:Err -> {error, Err}
    end.


terminate(#state{backend=Backend}) ->
    case dbus_proxy:call(Backend, ?BACKEND_IFACE, <<"terminate">>, []) of
		_ -> ok
    end.

save(#state{backend=Backend}=State, #occi_node{}=Node) ->
    ?info("[~p] save(~p)~n", [?MODULE, Node]),
    case dbus_proxy:call(Backend, ?BACKEND_IFACE, <<"save">>, [occi_renderer_dbus:render(Node)]) of
		ok ->
			{ok, State};
		{error, Err} ->
			{{error, Err}, State}
    end.

delete(#state{backend=Backend}=State, #occi_node{id=Uri}=Node) ->
    ?info("[~p] delete(~p)~n", [?MODULE, Node]),
    case dbus_proxy:call(Backend, ?BACKEND_IFACE, <<"delete">>, [occi_uri:to_binary(Uri)]) of
		ok ->
			{ok, State};
		{error, Err} ->
			{{error, Err}, State}
    end.


update(#state{backend=Backend}=State, #occi_node{}=Node) ->
    ?info("[~p] update(~p)~n", [?MODULE, Node]),
    case dbus_proxy:call(Backend, ?BACKEND_IFACE, <<"update">>, [occi_renderer_dbus:render(Node)]) of
		ok ->
			{ok, State};
		{error, Err} ->
			{{error, Err}, State}
    end.


find(#state{backend=Backend}=State, #occi_node{id=Uri}=_N) ->
    ?info("[~p] find(~p)~n", [?MODULE, _N]),
    case dbus_proxy:call(Backend, ?BACKEND_IFACE, <<"find">>, [occi_uri:to_binary(Uri)]) of
		{ok, [Node]} ->
			{{ok, [occi_parser_dbus:parse(Node)]}, State};
		{error, Err} ->
			{{error, Err}, State}
    end.


load(#state{backend=Backend}=State, #occi_node{id=Uri}=Node, _Opts) ->
    ?info("[~p] load(~p)~n", [?MODULE, Uri]),
    case dbus_proxy:call(Backend, ?BACKEND_IFACE, <<"load">>, [occi_renderer_dbus:render(Node)]) of
		{ok, N} ->
			{{ok, occi_parser_dbus:parse(N)}, State};
		{error, Err} ->
			{{error, Err}, State}
    end.


action(#state{backend=Backend}=State, #uri{}=Id, #occi_action{}=A) ->
    ?info("[~p] action(~p, ~p)~n", [?MODULE, Id, A]),
    Args = [occi_renderer_dbus:render(Id), 
			occi_renderer_dbus:render(A)],
    case dbus_proxy:call(Backend, ?BACKEND_IFACE, <<"load">>, Args) of
		ok ->
			{ok, State};
		{error, Err} ->
			{{error, Err}, State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_opts(Props) ->
    Srv = case proplists:get_value(service, Props) of
			  undefined -> throw({error, {missing_opt, service}});
			  Str -> list_to_binary(Str)
		  end,
	Opts = case proplists:get_value(opts, Props) of
			   undefined -> [];
			   V -> V
		   end,
	{Srv, Opts}.
    

connect_backend(Service) ->
    case dbus_bus_connection:connect(session) of
		{ok, Bus} ->
			case dbus_proxy:start_link(Bus, Service) of
				{ok, Backend} -> {ok, Bus, Backend};
				{error, _} = Err -> Err
			end;
		{error, _} = Err -> Err
    end.


process_schemas([], Acc) ->
    lists:reverse(Acc);

process_schemas([#dbus_variant{type=string, value=Bin} | Rest], Acc) ->
    process_schemas(Rest, [Bin | Acc]);

process_schemas([#dbus_variant{type={struct, [string, string]}, value={<<"path">>, Val}} | Rest], Acc) ->
    process_schemas(Rest, [{path, Val} | Acc]).
