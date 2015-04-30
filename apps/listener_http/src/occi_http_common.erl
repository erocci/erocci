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

-include("occi_http.hrl").
-include("occi.hrl").
-include("occi_log.hrl").
-include_lib("kernel/include/inet.hrl").

% API
-export([start/3,
	 stop/1,
	 set_cors/2,
	 get_routes/1,
	 set_routes/2,
	 auth/2,
	 get_acl_op/1,
	 get_auth/0]).

-define(DEFAULT_POOL, 10).
-define(ROUTE_OCCI(X),    {<<"/[...]">>,                       occi_http_handler,  X}).

start(Ref, Protocol, Props) ->
    application:ensure_all_started(cowboy),
    application:ensure_all_started(occi_authnz),
    case ets:info(Ref) of
	undefined ->
	    Ref = ets:new(Ref, [set, public, {keypos, 1}, named_table]),
	    ets:insert(Ref, {routes, []});
	_ -> ok
    end,
    Pool = proplists:get_value(pool, Props, ?DEFAULT_POOL),
    case proplists:get_value(auth, Props) of
	undefined ->
	    Routes = lists:flatten([ets:lookup_element(Ref, routes, 2), get_occi_handler(undefined)]),
	    Env = [{dispatch, cowboy_router:compile([{'_', Routes}])}],
	    cowboy:Protocol(Ref, Pool, Props, [{env, Env}]);
	{AuthMod, AuthOpts} ->
	    case occi_authnz:start_link(AuthMod, AuthOpts) of
		{ok, Pid} ->
		    Routes = lists:flatten([ets:lookup_element(Ref, routes, 2), get_occi_handler(Pid)]),
		    Env = [{dispatch, cowboy_router:compile([{'_', Routes}])}],
		    cowboy:Protocol(Ref, Pool, Props, [{env, Env}, {auth, Pid}]);
		ignore ->
		    throw({error, invalid_authentication_backend});
		{error, Err} ->
		    throw({error, Err})
	    end
    end.


stop(Ref) ->
    cowboy:stop_listener(Ref).


% Convenience function for setting CORS headers
-define(EXPOSE_HEADERS, <<"server,category,link,x-occi-attribute,x-occi-location,location">>).
set_cors(Req, Methods) ->
    case cowboy_req:header(<<"origin">>, Req) of
	{undefined, Req1} -> 
	    Req1;
	{Origin, Req1} ->
	    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, Methods, Req1),
	    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req2),
	    cowboy_req:set_resp_header(<<"access-control-expose-headers">>, ?EXPOSE_HEADERS, Req3)
    end.

-spec get_routes(Ref :: atom()) -> [term()].
get_routes(Ref) ->
    Opts = ranch:get_protocol_options(Ref),
    AuthRef = proplists:get_value(auth, Opts),
    lists:flatten([ets:lookup_element(Ref, routes, 2), get_occi_handler(AuthRef)]).

-spec set_routes(Ref :: atom(), Routes :: list()) -> ok.
set_routes(Ref, Routes) ->
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    Opts = ranch:get_protocol_options(Ref),
    Env = proplists:get_value(env, Opts),
    Env2 = [{dispatch, Dispatch} | proplists:delete(dispatch, Env)],
    ok = ranch:set_protocol_options(Ref, [{env, Env2} | proplists:delete(env, Opts)]).

-spec auth(reference(), term()) -> {true, occi_user()} | false.
auth(Ref, Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
	{undefined, _} -> false;
	{Value, _} -> 
	    case parse_auth(Value) of
		{basic, Auth} ->
		    get_basic_user(Ref, Auth);
		{digest, _Auth} ->
		    ?info("Unsupported authentication method: digest"),
		    false;
		{error, Err} ->
		    ?debug("Parse error: ~p~n", [Err]),
		    false
	    end
    end.

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

get_basic_user(Ref, Auth) ->
    case binary:split(base64:decode(Auth), [<<":">>]) of
	[User, Passwd] -> 
	    case occi_authnz:auth(Ref, {User, Passwd}) of
		true -> {true, User};
		false -> false
	    end;
	_ -> false
    end.

-spec parse_auth(binary()) -> {basic | digest | error, term()}.
parse_auth(Bin) ->
    parse_method(parse_next(Bin)).

parse_method({Method, Rest}) ->
    case to_lower(Method) of
	<<"basic">> -> parse_basic_hash(Rest);
	<<"digest">> -> parse_digest(Rest);
	M -> {error, {invalid_method, M}}
    end.

%%%
%%% Priv
%%%
get_occi_handler(AuthRef) ->
    ?ROUTE_OCCI([{auth, AuthRef}]).

parse_basic_hash(Bin) ->
    {basic, Bin}.

parse_digest(Bin) ->
    {digest, Bin}.

parse_next(Bin) ->
    case binary:split(Bin, <<" ">>, [trim]) of
	[<<>>, Rest] -> parse_next(Rest);
	[Next, Rest] -> {Next, Rest};
	[Next] -> {Next, <<>>}
    end.

to_lower(Bin) ->
    to_lower(Bin, <<>>).

to_lower(<<>>, Acc) ->
    Acc;
to_lower(<< C, Rest/bits >>, Acc) ->
    case C of
	$A -> to_lower(Rest, << Acc/binary, $a >>);
	$B -> to_lower(Rest, << Acc/binary, $b >>);
	$C -> to_lower(Rest, << Acc/binary, $c >>);
	$D -> to_lower(Rest, << Acc/binary, $d >>);
	$E -> to_lower(Rest, << Acc/binary, $e >>);
	$F -> to_lower(Rest, << Acc/binary, $f >>);
	$G -> to_lower(Rest, << Acc/binary, $g >>);
	$H -> to_lower(Rest, << Acc/binary, $h >>);
	$I -> to_lower(Rest, << Acc/binary, $i >>);
	$J -> to_lower(Rest, << Acc/binary, $j >>);
	$K -> to_lower(Rest, << Acc/binary, $k >>);
	$L -> to_lower(Rest, << Acc/binary, $l >>);
	$M -> to_lower(Rest, << Acc/binary, $m >>);
	$N -> to_lower(Rest, << Acc/binary, $n >>);
	$O -> to_lower(Rest, << Acc/binary, $o >>);
	$P -> to_lower(Rest, << Acc/binary, $p >>);
	$Q -> to_lower(Rest, << Acc/binary, $q >>);
	$R -> to_lower(Rest, << Acc/binary, $r >>);
	$S -> to_lower(Rest, << Acc/binary, $s >>);
	$T -> to_lower(Rest, << Acc/binary, $t >>);
	$U -> to_lower(Rest, << Acc/binary, $u >>);
	$V -> to_lower(Rest, << Acc/binary, $v >>);
	$W -> to_lower(Rest, << Acc/binary, $w >>);
	$X -> to_lower(Rest, << Acc/binary, $x >>);
	$Y -> to_lower(Rest, << Acc/binary, $y >>);
	$Z -> to_lower(Rest, << Acc/binary, $z >>);
	_ -> to_lower(Rest, << Acc/binary, C >>)
    end.
