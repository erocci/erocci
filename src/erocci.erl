%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (c) 2015-2016 Jean Parpaillon
%%% 
%%% This file is provided to you under the license described
%%% in the file LICENSE at the root of the project.
%%%
%%% You can also download the LICENSE file from the following URL:
%%% https://github.com/erocci/erocci/blob/master/LICENSE
%%% 

%% @doc erocci startup code
-module(erocci).
-author('Jean Parpaillon <jean.parpaillon@free.fr>').

-behaviour(application).

-include_lib("erocci_core/include/erocci_log.hrl").

-export([start/0, 
		 stop/0]).

-export([start/2, stop/1]).

%% @spec start() -> ok
%% @doc Start the occi server.
start() ->
    ?info("Starting erocci framework"),
    {ok, _} = application:ensure_all_started(erocci).

%% @spec stop() -> ok
%% @doc Stop the occi server.
stop() ->
    ?info("Stopping erocci framework"),
    application:stop(erocci).


start(normal, _Args) ->
	erocci_sup:start_link();

start(_StartType, _StartArgs) ->
	{error, badarg}.


stop(_State) ->
	ok.
