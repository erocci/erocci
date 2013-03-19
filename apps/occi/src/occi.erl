%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2013 Jean Parpaillon.

%% @doc occi startup code

-module(occi).
-author('Jean Parpaillon <jean.parpaillon@free.fr>').
-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the occi server.
start() ->
		ok = application:start(crypto),
		ok = application:start(ranch),
		ok = application:start(cowboy),
		ok = application:start(occi).

%% @spec stop() -> ok
%% @doc Stop the occi server.
stop() ->
    Res = application:stop(occi),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto),
    Res.
