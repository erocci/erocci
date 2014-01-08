%%%-------------------------------------------------------------------
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
%%% Created : 25 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_http).
-compile({parse_transform, lager_transform}).

-behaviour(occi_listener).

-include("occi.hrl").

% API
-export([add_collection/2, set_cors/1]).

-export([onresponse_debug/4]).

%% occi_listener callbacks
-export([start_link/1, terminate/0]).

-define(QUERY_ROUTES(Opts), [{<<"/-/">>,                          occi_http_query,    Opts},
			     {<<"/.well-known/org/ogf/occi/-/">>, occi_http_query,    Opts}]).
-define(ENTITY_ROUTE,       {<<"/[...]">>,                       occi_http_entity,   []}).

-define(TABLE, ?MODULE).

-record(route, {path     :: binary(),
		handler  :: atom(),
		opts     :: term()}).

start_link(Opts) ->
    application:start(cowlib),
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    lager:info("Starting HTTP listener ~p~n", [Opts]),
    Dispatch = init(),
    cowboy:start_http(http, 100, validate_cfg(Opts), [{env, [{dispatch, Dispatch}]}]).

onresponse_debug(Code, Headers, Body, Req) ->
    lager:debug("### Code: ~p~n", [Code]),
    lager:debug("### Headers: ~p~n", [Headers]),
    lager:debug("### Body: ~p~n", [Body]),
    lager:debug("### Req: ~p~n", [Req]),
    Req.

-spec add_collection(occi_category(), uri()) -> ok.
add_collection(Category, Uri) ->
    Route = #route{path=occi_types:join_path([<<"">>|Uri]),
		   handler=occi_http_collection,
		   opts=Category},
    ets:insert(?TABLE, Route),
    cowboy:set_env(http, dispatch, get_dispatch()).

validate_cfg(Opts) ->
    Address = case lists:keyfind(ip, 1, Opts) of
     		  false ->
     		      {0,0,0,0};
     		  {ip, Str} ->
     		      case inet_parse:address(binary_to_list(Str)) of
     			  {ok, Ip} -> Ip;
     			  {error, einval} -> 
     			      lager:error("Invalid listener address: ~p~n", [Str]),
     			      throw(einval)
     		      end
     	      end,
    Port = case lists:keyfind(port, 1, Opts) of
	       false ->
		   lager:error("No port in listener config: ~p~n", [?MODULE]),
		   throw(einval);
	       {port, I} -> I
	   end,
    [{ip, Address}, {port, Port}].

terminate() ->
    cowboy:stop_listener(http).

% Convenience function for setting CORS headers
set_cors(Req) ->
    case cowboy_req:header(<<"origin">>, Req) of
	{undefined, Req1} -> 
	    Req1;
	{Origin, Req1} ->
	    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, 
				      <<"HEAD, GET, PUT, POST, OPTIONS, DELETE">>, 
				      Req1),
	    cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req2)
    end.

%%%
%%% Private
%%%
init() ->
    ?TABLE = ets:new(?TABLE, [set, public, {keypos, 2}, named_table]),
    Routes = lists:map(fun (#occi_kind{location=Uri}=Kind) ->
			       #route{path=Uri, handler=occi_http_collection, opts=Kind};
			   (#occi_mixin{location=Uri}=Mixin) ->
			       #route{path=Uri, handler=occi_http_collection, opts=Mixin}			   
		       end, occi_category_mgr:get_categories()),
    ets:insert(?TABLE, Routes),
    get_dispatch().

get_dispatch() ->
    CollectionRoutes = ets:match_object(?TABLE, {route, '_', '_', '_'}),
    Routes = lists:flatten([?QUERY_ROUTES([]),
     			    [ {R#route.path, R#route.handler, R#route.opts} || R <- CollectionRoutes ],
     			    ?ENTITY_ROUTE]),
    cowboy_router:compile([{'_', Routes}]).
