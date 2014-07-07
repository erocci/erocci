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
-include_lib("kernel/include/inet.hrl").

-define(HTPASSWD, "priv/htpasswd").
-define(TBL, ?MODULE).

% API
-export([start/1,
	 stop/0,
	 set_cors/2,
	 add_route/2,
	 get_dispatch/0,
	 auth/1,
	 get_acl_op/1,
	 get_auth/0]).

-define(ROUTE_OCCI,    {<<"/[...]">>,                       occi_http_handler,  []}).

start(Props) ->
    occi:ensure_started(crypto),
    occi:ensure_started(cowlib),
    occi:ensure_started(ranch),
    occi:ensure_started(cowboy),
    occi:ensure_started(epasswd),
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
    Routes = lists:flatten([ets:lookup_element(?TBL, routes, 2), ?ROUTE_OCCI]),
    cowboy_router:compile([{'_', Routes}]).

-spec auth(term()) -> {true, occi_user()} | false.
auth(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
	{undefined, _} -> false;
	{Value, _} -> 
	    case parse_auth(Value) of
		{basic, Auth} ->
		    get_basic_user(Auth);
		{digest, _Auth} ->
		    lager:info("Unsupported authentication method: digest"),
		    false;
		{error, Err} ->
		    lager:debug("Parse error: ~p~n", [Err]),
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
    occi_config:set(name, occi_uri:parse(list_to_binary(Name))).

pattern_name(Props) ->
    case occi_config:get(name) of
	undefined ->
	    set_name(Props);
	_Name ->
	    ok
    end.

get_basic_user(Auth) ->
    case binary:split(base64:decode(Auth), [<<":">>]) of
	[User, Passwd] -> 
	    case epasswd:auth({User, Passwd}) of
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
