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
%% @doc Example webmachine_resource.

-module(occi_http_entity).
-compile({parse_transform, lager_transform}).

%% REST Callbacks
-export([init/3, 
	 rest_init/2,
	 resource_exists/2,
	 content_types_provided/2]).

%% Callback callbacks
-export([to_json/2]).

-include("occi.hrl").

-record(state, {entity = undefined :: any()}).

init(_Transport, _Req, []) -> 
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    Req1 = occi_http:set_cors(Req),
    {ok, Req1, #state{}}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>,       <<"json">>,     []}, to_json}
     ],
     Req, State}.

resource_exists(Req, State) ->
    {Path, _} = cowboy_req:path(Req),
    Uri = occi_config:get_url(Path),
    lager:debug("Check path: ~p~n", [occi_uri:to_string(Uri)]),
    case occi_store:find(#occi_entity{id=Uri, _='_'}) of
	{ok, []} ->
	    {false, Req, State};
	{ok, [Entity]} ->
	    {true, Req, State#state{entity=Entity}};
	{ok, _} ->
	    {false, cowboy_req:reply(500, Req), State}
    end.

to_json(Req, #state{entity=Entity}=State) ->
    Body = occi_renderer_json:render_entity(Entity),
    {[Body, "\n"], Req, State}.

%%%
%%% Private
%%%
