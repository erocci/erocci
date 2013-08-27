%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2013 Jean Parpaillon.

%% @doc occi startup code
-module(occi).
-author('Jean Parpaillon <jean.parpaillon@free.fr>').

-compile({parse_transform, lager_transform}).

-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the occi server.
start() ->
    lager:info("Starting erocci framework"),
    application:start(occi).

%% @spec stop() -> ok
%% @doc Stop the occi server.
stop() ->
    lager:info("Stopping erocci framework"),
    application:stop(occi).
