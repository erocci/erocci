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
-module(occi_renderer_occi).

-behaviour(occi_renderer).

-include("occi.hrl").

%% API
-export([render/2,
	 render_headers/2]).

%%%===================================================================
%%% API
%%%===================================================================
render(Node, Env) ->
    occi_renderer_text:render(Node, Env, fun ?MODULE:render_headers/2).

render_headers(Headers, #occi_env{req=Req}=Env) ->
    Req2 = lists:foldl(fun (Name, Acc) -> 
			       Value = occi_renderer:join(
					 lists:reverse(orddict:fetch(Name, Headers)),
					 ", "),
			       cowboy_req:set_resp_header(Name, Value, Acc)
		       end, Req, orddict:fetch_keys(Headers)),
    {<<"OK\n">>, Env#occi_env{req=Req2}}.
