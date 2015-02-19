%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
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
%%% Created : 10 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_type).

-include("occi.hrl").
-include("occi_xml.hrl").

-export([check/2,
	 match/3]).

-spec check(occi_attr_type(), term()) -> term().
check({?xmlschema_ns, Type}, Val) ->
    check(Type, Val);

check(string, Val) ->
    to_string(Val);

check(integer, Val) ->
    to_integer(Val);

check(float, Val) ->
    to_float(Val);

check(anyURI, Val) ->
    to_uri(Val);

check(_, Val) ->
    to_string(Val).


-spec match(occi_attr_type(), term(), binary()) -> true | false.
match({?xmlschema_ns, Type}, Value, M) ->
    match(Type, Value, M);

match(string, Val, {'=:=', Val}) ->
    true;

match(string, Val, {like, Val}) ->
    true;

match(string, Val, {like, Match}) ->
    match_string(Val, Match);

match(integer, Val, {'=:=', M}) ->
    Val =:= to_integer(M);

match(float, Val, {'=:=', M}) ->
    Val =:= to_float(M);

match(anyURI, Val, {'=:=', M}) ->
    try occi_uri:parse(M) of
	Val -> true;
	_ -> false
    catch throw:_ -> false
    end;

match(_, Val, {'=:=', Val}) -> 
    true;

match(_, _, _) -> 
    false.


%%%
%%% Priv
%%%
to_uri(#uri{} = X) ->
    X;
to_uri(X) ->
    occi_uri:parse(X).

to_string(X) when is_list(X) ->
    list_to_binary(X);
to_string(X) when is_binary(X) ->
    X;
to_string(X) ->
    throw({error, {einval, X}}).

to_integer(X) when is_integer(X) ->
    X;
to_integer(X) when is_binary(X) ->
    try binary_to_integer(X) of
	I -> I
    catch
	_:_ ->
	    throw({error, {einval, X}})
    end;
to_integer(X) when is_list(X) ->
    try list_to_integer(X) of
	I -> I
    catch
	_:_ ->
	    throw({error, {einval, X}})
    end;
to_integer(X) ->
    throw({error, {einval, X}}).

to_float(X) when is_float(X) ->
    X;
to_float(X) when is_integer(X) ->
    X+0.0;
to_float(X) when is_binary(X) ->
    try binary_to_float(X) of
	V -> V
    catch 
	_:_ ->
	    try binary_to_integer(X) of
		V -> V+0.0
	    catch
		_:_ -> 
		    throw({error, {einval, X}})
	    end
    end;
to_float(X) when is_list(X) ->
    try list_to_float(X) of
	V -> V
    catch 
	_:_ ->
	    try list_to_integer(X) of
		V -> V+0.0
	    catch
		_:_ -> 
		    throw({error, {einval, X}})
	    end
    end;
to_float(X) ->
    throw({error, {einval, X}}).

match_string(undefined, _) -> false;
match_string(<<>>, <<>>) -> true;
match_string(_, <<>>) -> true;
match_string(<<>>, _) -> false;
match_string(<<C, Rest/bits>>, <<C, Rest2/bits>>) -> match_string(Rest, Rest2);
match_string(<<_, Rest/bits>>, M) -> match_string(Rest, M).
