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

-module(occi_http_collection).
-compile({parse_transform, lager_transform}).

%% REST Callbacks
-export([init/3, 
	 rest_init/2,
	 allow_missing_post/2,
	 allowed_methods/2,
	 content_types_provided/2,
	 content_types_accepted/2]).

%% Callback callbacks
-export([to_plain/2,
	 to_json/2,
	 from_json/2]).

-include("occi.hrl").

-record(state, {category     :: occi_category()}).

init(_Transport, _Req, _) -> 
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, Cat) ->
    Req1 = occi_http:set_cors(Req),
    {ok, Req1, #state{category=Cat}}.

allow_missing_post(_Req, _State) ->
    false.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"PUT">>, <<"DELETE">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"text">>,          <<"plain">>,     []}, to_plain},
      {{<<"application">>,   <<"json">>,      []}, to_json},
      {{<<"application">>,   <<"occi+json">>, []}, to_json}
     ],
     Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>,     <<"json">>,      []}, from_json},
      {{<<"application">>,     <<"occi+json">>, []}, from_json}
     ],
     Req, State}.

to_plain(Req, #state{category=Cat}=State) ->
    {ok, Entities} = occi_store:get_collection(Cat),
    Body = [occi_renderer_plain:render_collection(Entities), "\n"],
    {Body, Req, State}.

to_json(Req, #state{category=Cat}=State) ->
    {ok, Entities} = occi_store:get_collection(Cat),
    Body = [occi_renderer_json:render_collection(Entities), "\n"],
    {Body, Req, State}.

from_json(Req, #state{category=#occi_category{id=#occi_cid{class=kind}}=Cat}=State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case occi_parser_json:parse_resource(Body, Cat) of
	{error, Reason} ->
	    lager:debug("Error processing request: ~p~n", [Reason]),
	    {true, cowboy_req:reply(400, Req2), State};
	{ok, #occi_resource{}=Res} ->
	    {Host, Req3} = cowboy_req:host(Req2),
	    {Prefix, Req4} = cowboy_req:path(Req3),
	    Res2 = occi_resource:set_id(Res, occi_store:gen_id(Host, Prefix)),
	    case occi_store:create(Cat#occi_category.backend, Res2) of
		{ok, Res3} ->
		    RespBody = occi_renderer_json:render_collection([Res3]),
		    {true, cowboy_req:set_resp_body(RespBody, Req4), State};
		{error, Reason} ->
		    lager:debug("Error creating resource"),
		    throw({error, Reason})
	    end
    end;
from_json(Req, #state{category=#occi_category{id=#occi_cid{class=mixin}}=_Cat}=State) ->
    {ok, Req, State}.
