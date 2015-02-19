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
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_renderer_plain).

-behaviour(occi_renderer).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("occi.hrl").

%% API
-export([render/2]).
-export([render_headers/2]).

%%%===================================================================
%%% API
%%%===================================================================
render(Node, #occi_env{}=Env) ->
    occi_renderer_text:render(Node, Env, fun ?MODULE:render_headers/2).

render_headers(Headers, #occi_env{req=Req}=Env) ->
    {BodyHdr, Req2} = 
	lists:foldl(fun (Name, Acc) ->
			    dispatch_headers(Name, Acc, Headers)
		    end, {[], Req}, lists:reverse(orddict:fetch_keys(Headers))),
    {[ occi_renderer:join(BodyHdr, "\n") | "\n"], Env#occi_env{req=Req2}}.

%%
%% Private
%%
dispatch_headers(<<"location">>, {BodyAcc, ReqAcc}, Headers) ->
    Value = occi_renderer:join(orddict:fetch(<<"location">>, Headers), ", "),
    ReqAcc2 = cowboy_req:set_resp_header(<<"location">>, Value, ReqAcc),
    {BodyAcc, ReqAcc2};

dispatch_headers(Name, {BodyAcc, ReqAcc}, Headers) -> 
    {[ render_header(Name, orddict:fetch(Name, Headers)) | BodyAcc ], ReqAcc}.


render_header(Name, Values) ->
    occi_renderer:join(
      lists:foldl(fun ([], Acc) ->
			  Acc;
		      (Value, Acc) ->
			  [ [ Name, ": ", Value] | Acc]
		  end, [], Values),
      "\n").
