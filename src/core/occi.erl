%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2013 Jean Parpaillon.
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

%% @doc occi startup code
-module(occi).
-author('Jean Parpaillon <jean.parpaillon@free.fr>').

-compile({parse_transform, lager_transform}).

-export([start/0, 
	 stop/0,
	 config/1]).
-export([ensure_started/1]).

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

config(Cfg) ->
    occi_config:load(Cfg).

ensure_started(App) ->
    case application:start(App) of
	ok -> ok;
	{error, {already_started, App}} -> ok;
	{error, Err} -> {error, Err}
    end.
    
