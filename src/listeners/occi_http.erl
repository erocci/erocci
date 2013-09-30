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
-export([add_collection/3, set_cors/1]).

%% occi_listener callbacks
-export([start_link/1, terminate/0]).

-define(QUERY_ROUTE,  {<<"/-/">>,                          occi_http_query,    []}).
-define(QUERY_ROUTE2, {<<"/.well-known/org/ogf/occi/-/">>, occi_http_query,    []}).
-define(ENTITY_ROUTE, {<<"/[...]">>,                       occi_http_entity,   []}).

-record(route, {path     :: binary(),
		handler  :: atom(),
		opts     :: term()}).

start_link(Opts) ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    lager:info("Starting HTTP listener ~p~n", [Opts]),
    init(),
    {ok, _} = cowboy:start_http(http, 100, validate_cfg(Opts),
				[{env, [{dispatch, get_dispatch()}]}]
			       ),
    loop().

-spec add_collection(occi_cid(), reference(), uri()) -> ok.
add_collection(Id, Ref, Uri) ->
    Route = #route{path=occi_types:join_path([<<"">>|Uri]),
		   handler=occi_http_collection,
		   opts={Id, Ref}},
    mnesia:dirty_write(Route),
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

loop() ->
    receive
	stop ->
	    cowboy:stop_listener(http);
	_ ->
	    loop()
    end.

terminate() ->
    ?MODULE ! stop.

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
    mnesia:create_table(route,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, route)}]),
    mnesia:wait_for_tables([route],
			   infinite),
    Entries = occi_category_mgr:get_entries(),
    mnesia:transaction(fun () ->
			       lists:foreach(fun ({category_entry, Id, Ref, Uri}) -> 
						     Route = #route{path=occi_types:join_path([<<"">>|Uri]),
								    handler=occi_http_collection,
								    opts={Id, Ref}},
						     mnesia:write(Route)
					     end, Entries)
		       end).

get_dispatch() ->
    CollectionRoutes = mnesia:dirty_match_object(#route{_='_'}),
    Routes = lists:flatten([?QUERY_ROUTE,
     			    ?QUERY_ROUTE2,
     			    [ {R#route.path, R#route.handler, R#route.opts} || R <- CollectionRoutes ],
     			    ?ENTITY_ROUTE]),
    cowboy_router:compile([{'_', Routes}]).
