%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_http).
-compile({parse_transform, lager_transform}).

-behaviour(occi_listener).

%% occi_listener callbacks
-export([start_link/1, terminate/0, validate_cfg/1]).

start_link(Opts) ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    lager:info("Starting HTTP listener ~p~n", [Opts]),
    Routes = [
	      {<<"/-/">>, occi_http_query, []},
	      {<<"/.well-known/org/ogf/occi/-/">>, occi_http_query, []}
	     ],
    %PrivRoutes = [ {<<"_priv">>, occi_priv, []} ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_http(http, 100, Opts,
				[{env, [{dispatch, Dispatch}]}]
			       ),
    ok.

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
    cowboy:stop_listener(http),
    ok.
