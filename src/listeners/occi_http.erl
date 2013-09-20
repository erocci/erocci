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

%% occi_listener callbacks
-export([start_link/1, terminate/0]).

start_link(Opts) ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    lager:info("Starting HTTP listener ~p~n", [Opts]),
    Opts2 = validate_cfg(Opts),
    Routes = [
	      {<<"/-/">>, occi_http_query, []},
	      {<<"/.well-known/org/ogf/occi/-/">>, occi_http_query, []},
	      {<<"/[...]">>, occi_http_all, []}
	     ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_http(http, 100, Opts2,
				[{env, [{dispatch, Dispatch}]}]
			       ),
    loop().

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
