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

-export([start/0, stop/0]).
% External API (simple wrappers)
-export([register_backend/2, register_category/3, register_listener/2]).

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

register_backend(Backend, Path) ->
    occi_store:register(Backend, Path).

register_category(Category, Location, Hooks) ->
    occi_category_mgr:register(Category, Location, Hooks).

register_listener(Listener, Args) ->
    occi_listener:register(Listener, Args).
