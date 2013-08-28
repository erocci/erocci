%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Generic type checking
%%%
%%% @end
%%% Created : 26 Jul 2013 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_types).

-export([is_enum/2, is_integer/1, is_float/1, is_alnum/1, is_cidr/1, is_ipaddress/1]).

-spec is_enum(any(), [any()]) -> {ok, any()} | error.
is_enum(Obj, [ Val | _Values]) when Obj == Val ->
    {ok, Obj};
is_enum(Obj, [ Val | Values]) when Obj /= Val ->
    is_enum(Obj, Values);
is_enum(_Obj, []) ->
    error.

-spec is_integer(any()) -> {ok, integer()} | error.
is_integer(Obj) ->
    try 
	Val = string:to_integer(Obj),
	{ok, Val}
    catch
	_ -> error
    end.

-spec is_float(any()) -> {ok, float()} | error.
is_float(Obj) ->
    try 
	Val = string:to_float(Obj),
	{ok, Val}
    catch
	_ -> error
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
