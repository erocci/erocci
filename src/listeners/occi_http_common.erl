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
-module(occi_http_common).
-compile({parse_transform, lager_transform}).

-include("occi.hrl").
-include("occi_http.hrl").
-include("occi_acl.hrl").
-include_lib("kernel/include/inet.hrl").

-define(TBL, ?MODULE).

% API
-export([start/1,
	 stop/0,
	 set_cors/2,
	 add_route/2,
	 get_dispatch/0,
	 get_acl_user/1,
	 get_acl_op/1,
	 get_auth/0]).

-define(ROUTE_QUERY,   {<<"/-/">>,                          occi_http_query,    []}).
-define(ROUTE_QUERY2,  {<<"/.well-known/org/ogf/occi/-/">>, occi_http_query,    []}).
-define(ROUTE_OCCI,    {<<"/[...]">>,                       occi_http_handler,  []}).

start(Props) ->
    occi:ensure_started(cowlib),
    occi:ensure_started(crypto),
    occi:ensure_started(ranch),
    occi:ensure_started(cowboy),
    pattern_name(Props),
    case ets:info(?TBL) of
	undefined ->
	    ?TBL = ets:new(?TBL, [set, public, {keypos, 1}, named_table]),
	    ets:insert(?TBL, {routes, []});
	_ -> ok
    end.

stop() ->
    ok.

% Convenience function for setting CORS headers
set_cors(Req, Methods) ->
    case cowboy_req:header(<<"origin">>, Req) of
	{undefined, Req1} -> 
	    Req1;
	{Origin, Req1} ->
	    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, Methods, Req1),
	    cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req2)
    end.

-spec add_route(atom(), {binary(), atom(), list()}) -> ok | {error, term()}.
add_route(Ref, Route) ->
    Routes = ets:lookup_element(?TBL, routes, 2),
    ets:insert(?TBL, {routes, [Route | Routes]}),
    cowboy:set_env(Ref, dispatch, get_dispatch()).

get_dispatch() ->
    Routes = lists:flatten([?ROUTE_QUERY,
			    ?ROUTE_QUERY2,
			    ets:lookup_element(?TBL, routes, 2),
			    ?ROUTE_OCCI]),
    cowboy_router:compile([{'_', Routes}]).

-spec get_acl_user(term()) -> {ok, acl_user()} | {error, term()}.
get_acl_user(_Req) ->
    {ok, anonymous}.

-spec get_acl_op(term()) -> acl_op().
get_acl_op(Req) ->
    case cowboy_req:method(Req) of
	{<<"GET">>, _} -> read;
	{<<"PUT">>, _} -> create;
	{<<"POST">>, _} ->
	    case cowboy_req:qs_val(<<"action">>, Req) of
		{undefined, _} -> update;
		{Action, _} -> {action, Action}
	    end;
	{<<"DELETE">>, _} -> delete;
	{<<"OPTIONS">>, _} -> read
    end.

-spec get_auth() -> iodata().
get_auth() ->
    "basic realm=\"" ++ ?SERVER_ID ++ "\"".

%%%
%%% Private
%%%
set_name(Props) ->
    Scheme = proplists:get_value(scheme, Props),
    Port = proplists:get_value(port, Props),
    Host = case proplists:get_value(ip, Props) of
	       {0,0,0,0} ->
		   case inet_res:gethostbyaddr({127,0,0,1}) of
		       {ok, #hostent{h_name=S}} -> S;
		       {error, Err} -> throw({error, Err})
		   end;
	       Ip ->
		   case inet_res:gethostbyaddr(Ip) of
		       {ok, #hostent{h_name=S}} -> S;
		       {error, Err} -> throw({error, Err})
		   end
	   end,
    Name = atom_to_list(Scheme)++"://"++Host++":"++integer_to_list(Port),
    occi_config:set(name, occi_uri:parse(Name)).

pattern_name(Props) ->
    case occi_config:get(name) of
	undefined ->
	    set_name(Props);
	_Name ->
	    ok
    end.
