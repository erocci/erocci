%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2013 Jean Parpaillon.

%% @doc Callbacks for the occi application.

-module(occi_app).
-author('Jean Parpaillon <jean.parpaillon@free.fr>').

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for occi.
start(_Type, _StartArgs) ->
		Dispatch = cowboy_router:compile([
								   {'_', [
												 {"/-/", occi_query, []},
												 {"/.well-known/org/ogf/occi/-/", occi_query, []}
												 ]}
								 ]),
		{ok, _} = cowboy:start_http(http, 100, [{port, 9086}],
																[{env, [{dispatch, Dispatch}]}]
															 ),
    occi_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for occi.
stop(_State) ->
    ok.
