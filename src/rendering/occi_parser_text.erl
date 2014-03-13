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
%%% Created : 7 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_parser_text).
-compile({parse_transform, lager_transform}).

-include("occi_parser_text.hrl").

%% API
-export([parse_action/2,
	 parse_entity/2,
	 parse_user_mixin/2,
	 parse_collection/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_action(_Headers, #state{}=_State) ->
    {ok, occi_action:new()}.

parse_entity(Headers, #state{}=State) ->
    case parse_category(Headers, State) of
	{ok, #state{entity=Entity}} ->
	    {ok, Entity};
	{error, Err} ->
	    {error, Err}
    end.

parse_user_mixin(_Headers, #state{}=_State) ->
    {ok, occi_mixin:new()}.

parse_collection(Headers, #state{}=_State) ->
    Ret = occi_collection:new(),
    case orddict:find('x-occi-location', Headers) of
	{ok, Values} ->
	    case parse_o_l_values(Values, []) of
		{ok, Uris} ->
		    {ok, occi_collection:add_entities(Ret, Uris)};
		{error, Err} ->
		    {error, Err}
	    end;
	error ->
	    {ok, Ret}
    end.

%%%
%%% Priv
%%%
parse_o_l_values([], Acc) ->
    {ok, Acc};
parse_o_l_values([Bin | Values], Acc) ->
    try occi_uri:parse(Bin) of
	#uri{}=Uri ->
	    parse_o_l_values(Values, [Uri | Acc])
    catch
	throw:Err ->
	    {error, Err}
    end.

parse_category(H, S) ->
    case orddict:find(category, H) of
	{ok, Values} ->
	    parse_c_values(Values, H, S);
	error ->
	    {error, {parse_error, missing_category}}
    end.

parse_c_values([], _H, #state{entity=undefined}=_S) ->
    {error, {parse_error, undefined_category}};
parse_c_values([], H, #state{entity=#occi_resource{}}=S) ->
    parse_attributes(H, S);
parse_c_values([], H, #state{entity=#occi_link{}}=S) ->
    parse_attributes(H, S);
parse_c_values([Bin | Values], H, S) ->
    parse_c_value(Bin, Values, H, S).

parse_c_value(Bin, V, H, S) ->
    case match_eq(Bin, 0) of
	nomatch ->
	    parse_c_term(Bin, V, H, S, <<>>);
	_ ->
	    {error, {parse_error, bad_term}}
    end.

parse_c_term(<< $;, Rest/bits >>, V, H, S, SoFar)->
    Term = list_to_atom(binary_to_list(SoFar)),
    parse_c_scheme(Rest, Term, V, H, S);
parse_c_term(<< C, Rest/bits >>, V, H, S, SoFar) ->
    parse_c_term(Rest, V, H, S, << SoFar/binary, C >>);
parse_c_term(_, _, _, _, _) ->
    {error, {parse_error, invalid_category}}.

parse_c_scheme(Bin, T, V, H, S) ->
    case parse_kv(Bin) of
	{ok, Rest, <<"scheme">>, {string, Value}} ->
	    Scheme = list_to_atom(binary_to_list(Value)),
	    parse_c_class(Rest, T, Scheme, V, H, S);
	{ok, _, _, _} ->
	    {error, {parse_error, category}};
	{error, Err} ->
	    {error, Err}
    end.

parse_c_class(Bin, Term, Scheme, V, H, S) ->
    Bin2 = parse_ws(Bin),
    case parse_kv(Bin2) of
	{ok, _, <<"class">>, {string, Value}} ->
	    Class = list_to_atom(binary_to_list(Value)),
	    add_category(Term, Scheme, Class, V, H, S);
	{ok, _, _, _} ->
	    {error, {parse_error, category}};
	{error, Err} ->
	    {error, Err}
    end.

add_category(Term, Scheme, Class, V, H, S) ->
    case occi_category_mgr:get(make_cid(Term, Scheme, Class)) of
	#occi_kind{}=Kind ->
	    new_entity(Kind, V, H, S);
	#occi_mixin{}=Mixin ->
	    add_mixin(Mixin, V, H, S)
    end.

make_cid(Term, Scheme, Class) when is_atom(Term), is_atom(Scheme), is_atom(Class) ->
    #occi_cid{scheme=Scheme, term=Term, class=Class}.

new_entity(Kind, V, H, #state{entity=undefined, entity_id=Id}=State) ->
    parse_c_values(V, H, State#state{entity=occi_entity:new(Id, Kind)}).

add_mixin(_, _, _, #state{entity=undefined}) ->
    {error, {parse_error, invalid_mixin}};
add_mixin(Mixin, V, H, #state{entity=Entity}=State) ->
    parse_c_values(V, H, State#state{entity=occi_entity:add_mixin(Entity, Mixin)}).

parse_attributes(Headers, State) ->
    case orddict:find('x-occi-attribute', Headers) of
	{ok, Attrs} ->
	    parse_a_values(Attrs, Headers, State);
	error ->
	    {ok, State}
    end.

parse_a_values([], H, #state{entity_id=undefined}=State) ->
    parse_location(H, State);
parse_a_values([], _, State) ->
    {ok, State};
parse_a_values([Bin | Attrs], H, S) ->
    parse_a_value(Bin, Attrs, H, S).

parse_a_value(Bin, V, H, S) ->
    case parse_kv(Bin) of
	{ok, _, Key, {_Type, Value}} ->
	    add_attr(list_to_atom(binary_to_list(Key)), Value, V, H, S);
	{error, Err} ->
	    {error, Err}
    end.

add_attr(_, _, _, _, #state{entity=undefined}) ->
    {error, {parse_error, attribute}};
add_attr(Key, Value, Attrs, H, #state{entity=E}=S) ->
    try occi_entity:set_attr_value(E, Key, Value) of
	#occi_resource{}=E2 ->
	    parse_a_values(Attrs, H, S#state{entity=E2});
	#occi_link{}=E2 ->
	    parse_a_values(Attrs, H, S#state{entity=E2});
	{error, Err} ->
	    {error, {parse_error, Err}}
    catch
	_:Err ->
	    {error, {parse_error, Err}}
    end.

parse_location(Headers, #state{entity=E}=S) ->
    case orddict:find(location, Headers) of
	{ok, [Location]} ->
	    try occi_uri:parse(Location) of
		#uri{}=Uri ->
		    {ok, S#state{entity=occi_entity:set_id(E, Uri)}}
	    catch
		_:Err ->
		    {error, {parse_error, Err}}
	    end;
	error ->
	    {error, {parse_error, no_location}}
    end.

parse_kv(Bin) ->
    Rest = parse_ws(Bin),
    parse_key(Rest, <<>>).

parse_key(<< $=, Rest/bits >>, SoFar) ->
    Rest2 = parse_ws(Rest),
    parse_value(Rest2, SoFar);
parse_key(<< C, Rest/bits >>, SoFar) ->
    case C of
	$a -> parse_key(Rest, << SoFar/binary, $a >>);
	$b -> parse_key(Rest, << SoFar/binary, $b >>);
	$c -> parse_key(Rest, << SoFar/binary, $c >>);
	$d -> parse_key(Rest, << SoFar/binary, $d >>);
	$e -> parse_key(Rest, << SoFar/binary, $e >>);
	$f -> parse_key(Rest, << SoFar/binary, $f >>);
	$g -> parse_key(Rest, << SoFar/binary, $g >>);
	$h -> parse_key(Rest, << SoFar/binary, $h >>);
	$i -> parse_key(Rest, << SoFar/binary, $i >>);
	$j -> parse_key(Rest, << SoFar/binary, $j >>);
	$k -> parse_key(Rest, << SoFar/binary, $k >>);
	$l -> parse_key(Rest, << SoFar/binary, $l >>);
	$m -> parse_key(Rest, << SoFar/binary, $m >>);
	$n -> parse_key(Rest, << SoFar/binary, $n >>);
	$o -> parse_key(Rest, << SoFar/binary, $o >>);
	$p -> parse_key(Rest, << SoFar/binary, $p >>);
	$q -> parse_key(Rest, << SoFar/binary, $q >>);
	$r -> parse_key(Rest, << SoFar/binary, $r >>);
	$s -> parse_key(Rest, << SoFar/binary, $s >>);
	$t -> parse_key(Rest, << SoFar/binary, $t >>);
	$u -> parse_key(Rest, << SoFar/binary, $u >>);
	$v -> parse_key(Rest, << SoFar/binary, $v >>);
	$w -> parse_key(Rest, << SoFar/binary, $w >>);
	$x -> parse_key(Rest, << SoFar/binary, $x >>);
	$y -> parse_key(Rest, << SoFar/binary, $y >>);
	$z -> parse_key(Rest, << SoFar/binary, $z >>);
	$0 -> parse_key(Rest, << SoFar/binary, $0 >>);
	$1 -> parse_key(Rest, << SoFar/binary, $1 >>);
	$2 -> parse_key(Rest, << SoFar/binary, $2 >>);
	$3 -> parse_key(Rest, << SoFar/binary, $3 >>);
	$4 -> parse_key(Rest, << SoFar/binary, $4 >>);
	$5 -> parse_key(Rest, << SoFar/binary, $5 >>);
	$6 -> parse_key(Rest, << SoFar/binary, $6 >>);
	$7 -> parse_key(Rest, << SoFar/binary, $7 >>);
	$8 -> parse_key(Rest, << SoFar/binary, $8 >>);
	$9 -> parse_key(Rest, << SoFar/binary, $9 >>);
	$. -> parse_key(Rest, << SoFar/binary, $. >>);
	C ->
	    {error, {parse_error, invalid_key}}
    end.

parse_value(<< $", Rest/bits >>, K) ->
    parse_string(Rest, K, <<>>);
parse_value(<< C, Rest/bits >>, K) ->
    case C of
	$0 -> parse_number(<< Rest/binary >>, << $0 >>, K);
	$1 -> parse_number(<< Rest/binary >>, << $1 >>, K);
	$2 -> parse_number(<< Rest/binary >>, << $2 >>, K);
	$3 -> parse_number(<< Rest/binary >>, << $3 >>, K);
	$4 -> parse_number(<< Rest/binary >>, << $4 >>, K);
	$5 -> parse_number(<< Rest/binary >>, << $5 >>, K);
	$6 -> parse_number(<< Rest/binary >>, << $6 >>, K);
	$7 -> parse_number(<< Rest/binary >>, << $7 >>, K);
	$8 -> parse_number(<< Rest/binary >>, << $8 >>, K);
	$9 -> parse_number(<< Rest/binary >>, << $9 >>, K);
	$+ -> parse_number(<< Rest/binary >>, << $+ >>, K);
	$- -> parse_number(<< Rest/binary >>, << $- >>, K);
	$. -> parse_float(<< Rest/binary >>, << $. >>, K);
	_ -> 
	    {error, {parse_error, value}}
    end.

parse_string(<< $", Rest/bits >>, K, V) ->
    Rest2 = parse_ws(Rest),
    parse_string_end(Rest2, K, V);
parse_string(<< C, Rest/bits >>, K, SoFar) ->
    parse_string(Rest, K, << SoFar/binary, C >>).

parse_string_end(<<>>, K, V) ->
    {ok, <<>>, K, {string, V}};
parse_string_end(<< $;, Rest/bits >>, K, V) ->
    {ok, Rest, K, {string, V}};
parse_string_end(_, _, _) ->
    {error, {parse_error, value}}.

parse_number(<<>>, SoFar, K) ->
    {ok, <<>>, K, {integer, binary_to_integer(SoFar)}};
parse_number(<< C, Rest/bits >>, SoFar, K) ->
    case C of
	$0 -> parse_number(<< Rest/binary >>, << SoFar/binary, $0 >>, K);
	$1 -> parse_number(<< Rest/binary >>, << SoFar/binary, $1 >>, K);
	$2 -> parse_number(<< Rest/binary >>, << SoFar/binary, $2 >>, K);
	$3 -> parse_number(<< Rest/binary >>, << SoFar/binary, $3 >>, K);
	$4 -> parse_number(<< Rest/binary >>, << SoFar/binary, $4 >>, K);
	$5 -> parse_number(<< Rest/binary >>, << SoFar/binary, $5 >>, K);
	$6 -> parse_number(<< Rest/binary >>, << SoFar/binary, $6 >>, K);
	$7 -> parse_number(<< Rest/binary >>, << SoFar/binary, $7 >>, K);
	$8 -> parse_number(<< Rest/binary >>, << SoFar/binary, $8 >>, K);
	$9 -> parse_number(<< Rest/binary >>, << SoFar/binary, $9 >>, K);
	$. -> parse_float(<< Rest/binary >>, << $. >>, K);
	$; -> {ok, Rest, K, {integer, binary_to_integer(SoFar)}};
	$\s -> {ok, Rest, K, {integer, binary_to_integer(SoFar)}};
	$\t -> {ok, Rest, K, {integer, binary_to_integer(SoFar)}};
	_ -> 
	    {error, {parse_error, value}}
    end.

parse_float(<<>>, SoFar, K) ->
    {ok, K, {float, binary_to_float(SoFar)}};
parse_float(<< C, Rest/bits >>, SoFar, K) ->
    case C of
	$0 -> parse_float(<< Rest/binary >>, << SoFar/binary, $0 >>, K);
	$1 -> parse_float(<< Rest/binary >>, << SoFar/binary, $1 >>, K);
	$2 -> parse_float(<< Rest/binary >>, << SoFar/binary, $2 >>, K);
	$3 -> parse_float(<< Rest/binary >>, << SoFar/binary, $3 >>, K);
	$4 -> parse_float(<< Rest/binary >>, << SoFar/binary, $4 >>, K);
	$5 -> parse_float(<< Rest/binary >>, << SoFar/binary, $5 >>, K);
	$6 -> parse_float(<< Rest/binary >>, << SoFar/binary, $6 >>, K);
	$7 -> parse_float(<< Rest/binary >>, << SoFar/binary, $7 >>, K);
	$8 -> parse_float(<< Rest/binary >>, << SoFar/binary, $8 >>, K);
	$9 -> parse_float(<< Rest/binary >>, << SoFar/binary, $9 >>, K);
	$; -> {ok, Rest, K, {float, binary_to_float(SoFar)}};
	$\s -> {ok, Rest, K, {float, binary_to_float(SoFar)}};
	$\t -> {ok, Rest, K, {float, binary_to_float(SoFar)}};
	_ ->
	    {error, {parse_error, value}}
    end.

parse_ws(<<>>) ->
    <<>>;
parse_ws(<< C, Rest/bits >>) ->
    case C of
	$\s -> parse_ws(Rest);
	$\t -> parse_ws(Rest);
	_ -> << C, Rest/binary >>
    end.

match_eq(<< $=, _/bits >>, N) ->
    N;
match_eq(<< $;, _/bits >>, _) ->
    nomatch;
match_eq(<< _, Rest/bits >>, N) ->
    match_eq(Rest, N + 1);
match_eq(_, _) ->
    nomatch.
