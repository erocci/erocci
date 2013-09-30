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
%%% @doc Generic type checking
%%%
%%% @end
%%% Created : 26 Jul 2013 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_types).

-include("occi.hrl").

-export([is_enum/2, is_integer/1, is_float/1, is_alnum/1, is_cidr/1, is_ipaddress/1]).
-export([is_range/2, is_module/1]).
-export([split_path/1, join_path/1]).

-spec is_enum(any(), [any()]) -> {ok, any()} | error.
is_enum(Obj, [ Val | _Values]) when Obj == Val ->
    {ok, Obj};
is_enum(Obj, [ Val | Values]) when Obj /= Val ->
    is_enum(Obj, Values);
is_enum(_Obj, []) ->
    error.

-spec is_integer(any()) -> {ok, integer()} | error.
is_integer(Obj) when erlang:is_integer(Obj) ->
    {ok, Obj};
is_integer(Obj) ->
    case string:to_integer(Obj) of
	{error, _} -> error;
	{Val, _Rest} -> {ok, Val}
    end.

-spec is_range(any(), [integer()]) -> {ok, integer()} | error.
is_range(Obj, [Min, Max]) ->
    case ?MODULE:is_integer(Obj) of
	{ok, Val} when Val >= Min, Val =< Max ->
	    {ok, Val};
	_ -> error
    end.	    

-spec is_float(any()) -> {ok, float()} | error.
is_float(Obj) when erlang:is_float(Obj) ->
    {ok, Obj};
is_float(Obj) ->
    case string:to_float(Obj) of
	{error, _} -> error;
	{Val, _Rest} -> {ok, Val}
    end.

-spec is_alnum(any()) -> {ok, list()} | error.
is_alnum(Obj) ->
    case re:run(Obj, "^[a-zA-Z0-9.-_]+$") of
	{match, Val} -> 
	    {ok, Val};
	nomatch -> 
	    error
    end.

is_cidr(Obj) ->
    {ok, Obj}.

is_ipaddress(Obj) ->
    {ok, Obj}.

is_module(Mod) when is_atom(Mod) ->
    try Mod:module_info() of
	_InfoList ->
	    true
    catch
	_:_ ->
	    false
    end;
is_module(_) ->
    false.

%% @doc Split a path into a list of path segments.
%%      Picked from cowboy_router
%%
%% Following RFC2396, this function may return path segments containing any
%% character, including <em>/</em> if, and only if, a <em>/</em> was escaped
%% and part of a path segment.
-spec split_path(binary()) -> uri().
split_path(<< $/, Path/bits >>) ->
	split_path(Path, []);
split_path(_) ->
	throw({error, einval}).

split_path(Path, Acc) ->
	try
		case binary:match(Path, <<"/">>) of
			nomatch when Path =:= <<>> ->
				lists:reverse([cowboy_http:urldecode(S) || S <- Acc]);
			nomatch ->
				lists:reverse([cowboy_http:urldecode(S) || S <- [Path|Acc]]);
			{Pos, _} ->
				<< Segment:Pos/binary, _:8, Rest/bits >> = Path,
				split_path(Rest, [Segment|Acc])
		end
	catch
	    error:badarg ->
		throw({error, einval})
	end.

-spec join_path(uri()) -> binary().
join_path(Uri) when is_list(Uri) ->
    << <<S/binary,$/>> || S <- Uri >>.


    
