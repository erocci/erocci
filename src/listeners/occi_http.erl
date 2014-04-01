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
-export([start_link/2,
	 terminate/2]).

start_link(Ref, Opts) ->
    lager:info("Starting HTTP listener ~p~n", [Opts]),
    O2 = validate_cfg(Opts),
    occi_http_common:start(O2),
    cowboy:start_http(Ref, 100, O2, [{env, [{dispatch, occi_http_common:get_dispatch()}]}]).

terminate(Ref, _Reason) ->
    occi_http_common:stop(),
    cowboy:stop_listener(Ref).

%%%
%%% Priv
%%%
validate_cfg(Opts) ->
    Address = proplists:get_value(ip, Opts, {0,0,0,0}),
    Port = proplists:get_value(port, Opts, 8080),
    [{ip, Address}, {port, Port}, {scheme, http}].
