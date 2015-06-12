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

-include("occi_parser_text.hrl").

%% API
-export([parse_action/2,
	 parse_entity/2,
	 parse_user_mixin/2,
	 parse_collection/2]).

-export([parse/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse(Headers, #state{type=action}=S) ->
    parse_action(Headers, S);

parse(Headers, #state{type=entity}=S) ->
    parse_entity(Headers, S);

parse(Headers, #state{type=user_mixin}=S) ->
    parse_user_mixin(Headers, S);

parse(Headers, #state{type=collection}=S) ->
    parse_collection(Headers, S).

parse_action(Headers, #state{}=State) ->
    case parse_category(Headers, State) of
	{ok, #state{action=Action}} ->
	    {ok, Action};
	{error, Err} ->
	    {error, {parse_error, Err}}
    end.

parse_entity(Headers, #state{}=State) ->
    case parse_category(Headers, State) of
	{ok, #state{entity=Entity}} ->
	    {ok, Entity};
	{error, Err} ->
	    {error, {parse_error, Err}}
    end.

parse_user_mixin(Headers, #state{}=State) ->
    case parse_category(Headers, State) of
	{ok, #state{mixin=Mixin}} ->
	    {ok, Mixin};
	{error, Err} ->
	    {error, {parse_error, Err}}
    end.

parse_collection(Headers, #state{}=_State) ->
    Ret = occi_collection:new(),
    case orddict:find('x-occi-location', Headers) of
	{ok, Values} ->
	    case parse_o_l_values(Values, []) of
		{ok, Uris} ->
		    {ok, occi_collection:add_entities(Ret, Uris)};
		{error, Err} ->
		    {error, {parse_error, Err}}
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
	    {error, missing_category}
    end.

parse_links(H, S) ->
    case orddict:find(link, H) of
	{ok, Values} ->
	    parse_l_values(Values, H, S);
	error ->
	    parse_attributes(H, S)
    end.

parse_l_values([], H, S) ->
    parse_attributes(H, S);
parse_l_values([Bin | Values], H, S) ->
    parse_l_open_target(Bin, Values, H, S).

parse_l_open_target(<< $<, Rest/bits >>, V, H, S) ->
    parse_l_target(Rest, V, H, S, <<>>);
parse_l_open_target(<< C, _/bits >>, _, _, _) ->
    {error, {invalid_char, C}}.

parse_l_target(<< $>, Rest/bits >>, V, H, S, SoFar) ->
    try occi_uri:parse(SoFar) of
	#uri{}=Target -> parse_l_after_target(parse_ws(Rest), Target, V, H, S)
    catch throw:Err -> {error, Err}
    end;
parse_l_target(<< C, Rest/bits >>, V, H, S, SoFar) ->
    parse_l_target(Rest, V, H, S, << SoFar/binary, C >>).

parse_l_after_target(<< $;, Rest/bits >>, T, V, H, S) ->
    Link = occi_link:new(),
    parse_l_kv(parse_kv(Rest), occi_link:set_target(Link, T), V, H, S);
parse_l_after_target(<< C, _/bits >>, _, _, _, _) ->
    {error, {invalid_char, C}}.

parse_l_kv({ok, rel, {string, Bin}, Rest}, L, V, H, S) ->
    try occi_cid:parse(Bin) of
	#occi_cid{}=Cid -> parse_l_kv(parse_kv(Rest), occi_link:set_target_cid(L, Cid#occi_cid{class=kind}), V, H, S)
    catch throw:Err -> {error, Err}
    end;
parse_l_kv({ok, rel, _, _}, _, _, _, _) ->
    {error, invalid_rel};
parse_l_kv({ok, self, {string, Bin}, Rest}, L, V, H, S) ->
    try occi_uri:parse(Bin) of
	#uri{}=Id -> parse_l_kv(parse_kv(Rest), occi_link:set_id(L, Id), V, H, S)
    catch throw:Err -> {error, Err}
    end;
parse_l_kv({ok, self, _, _}, _, _, _, _) ->
    {error, invalid_self};
parse_l_kv({ok, category, {string, Bin}, Rest}, L, V, H, S) ->
    Cid = occi_cid:parse(Bin),
    case occi_store:get(Cid#occi_cid{class='_'}) of
	{ok, #occi_kind{}=Kind} -> 
	    parse_l_kv(parse_kv(Rest), occi_link:set_cid(L, Kind), V, H, S);
	{ok, #occi_mixin{}=Mixin} -> 
	    parse_l_kv(parse_kv(Rest), occi_link:add_mixin(L, Mixin), V, H, S);
	_ -> 
	    throw({error, invalid_category})
    end;
parse_l_kv({ok, Key, {_, Val}, Rest}, L, V, H, S) ->
    case occi_link:set_attr_value(L, Key, Val) of
	#occi_link{}=L2 -> parse_l_kv(parse_kv(Rest), L2, V, H, S);
	{error, Err} -> {error, Err}
    end;
parse_l_kv({error, Err}, _, _, _, _) ->
    {error, Err};
parse_l_kv(none, L, V, H, #state{entity=#occi_resource{}=Res}=S) ->
    parse_l_values(V, H, S#state{entity=occi_resource:add_link(Res, L)}).

parse_c_values([], H, #state{type=filters}=S) ->
    parse_attributes(H, S);
parse_c_values([], _H, #state{entity=undefined}=_S) ->
    {error, {parse_error, undefined_category}};
parse_c_values([], H, #state{entity=#occi_resource{}}=S) ->
    parse_links(H, S);
parse_c_values([], H, #state{entity=#occi_link{}}=S) ->
    parse_attributes(H, S);
parse_c_values([Bin | Values], H, S) ->
    parse_c_value(Bin, Values, H, S).

parse_c_value(Bin, V, H, S) ->
    case match_eq(Bin, 0) of
	nomatch ->
	    parse_c_term(parse_ws(Bin), V, H, S, <<>>);
	_ ->
	    {error, bad_term}
    end.

parse_c_term(<< $;, Rest/bits >>, V, H, #state{mixin=#occi_mixin{id=Cid}}=S, SoFar)->
    Term = ?term_to_atom(SoFar),
    parse_c_kv(parse_kv(Rest), Cid#occi_cid{term=Term}, V, H, S);
parse_c_term(<< $;, Rest/bits >>, V, H, S, SoFar)->
    Term = ?term_to_atom(SoFar),
    parse_c_kv(parse_kv(Rest), #occi_cid{term=Term}, V, H, S);
parse_c_term(<< C, Rest/bits >>, V, H, S, SoFar) ->
    parse_c_term(Rest, V, H, S, << SoFar/binary, C >>);
parse_c_term(_, _, _, _, _) ->
    {error, invalid_category}.

parse_c_kv({ok, location, {string, Bin}, Rest}, Cid, V, H, #state{mixin=#occi_mixin{}=M}=S) ->
    try occi_uri:parse(Bin) of
	#uri{}=Uri ->
	    parse_c_kv(parse_kv(Rest), Cid, V, H, S#state{mixin=occi_mixin:set_location(M, Uri)})
    catch throw:Err -> {error, Err}
    end;
parse_c_kv({ok, scheme, {string, Bin}, Rest}, Cid, V, H, S) ->
    parse_c_kv(parse_kv(Rest), Cid#occi_cid{scheme=?scheme_to_atom(Bin)}, V, H, S);
parse_c_kv({ok, class, {string, Bin}, Rest}, #occi_cid{class=undefined}=Cid, V, H, S) ->
    parse_c_kv(parse_kv(Rest), Cid#occi_cid{class=?class_to_atom(Bin)}, V, H, S);
parse_c_kv({ok, class, {string, Bin}, Rest}, #occi_cid{class=Cls}=Cid, V, H, S) ->
    C = ?class_to_atom(Bin),
    if
	C == Cls -> parse_c_kv(parse_kv(Rest), Cid, V, H, S);
	true -> {error, invalid_class}
    end;
parse_c_kv({error, Err}, _, _, _, _) ->
    {error, Err};
parse_c_kv(none, Cid, [], H, #state{action=#occi_action{}}=S) ->
    case occi_store:get(Cid) of
	{ok, #occi_action{}=A} -> parse_attributes(H, S#state{action=A});
	_ -> {error, invalid_category}
    end;
parse_c_kv(none, Cid, [], _, #state{mixin=#occi_mixin{}=M}=S) ->
    {ok, S#state{mixin=M#occi_mixin{id=Cid}}};
parse_c_kv(none, Cid, V, H, S) ->
    add_category(Cid, V, H, S).

add_category(Cid, V, H, S) ->
    case occi_store:get(Cid) of
	{ok, #occi_kind{}=Kind} -> new_entity(Kind, V, H, S);
	{ok, #occi_mixin{}=Mixin} -> add_mixin(Mixin, V, H, S);
	_ ->
	    {error, {einval, Cid}}
    end.

new_entity(_, V, H, #state{entity=#occi_resource{}, entity_id=undefined}=State) ->
    parse_c_values(V, H, State);
new_entity(_, V, H, #state{entity=#occi_link{}, entity_id=undefined}=State) ->
    parse_c_values(V, H, State);
new_entity(Kind, V, H, #state{entity=undefined, entity_id=Id}=State) ->
    parse_c_values(V, H, State#state{entity=occi_entity:new(Id, Kind)}).

add_mixin(_, _, _, #state{entity=undefined}) ->
    {error, invalid_mixin};
add_mixin(Mixin, V, H, #state{entity=Entity}=State) ->
    parse_c_values(V, H, State#state{entity=occi_entity:add_mixin(Entity, Mixin)}).

parse_attributes(Headers, State) ->
    case orddict:find('x-occi-attribute', Headers) of
	{ok, Attrs} ->
	    parse_a_values(Attrs, Headers, State);
	error ->
	    {ok, State}
    end.

parse_a_values([], _, #state{action=#occi_action{}}=State) ->
    {ok, State};
parse_a_values([], _, #state{entity=#occi_resource{}=Res, entity_id=#uri{}=Id}=State) ->
    {ok, State#state{entity=occi_resource:set_id(Res, Id)}};
parse_a_values([], _, #state{entity=#occi_link{}=Link, entity_id=#uri{}=Id}=State) ->
    {ok, State#state{entity=occi_link:set_id(Link, Id)}};
parse_a_values([], H, #state{entity_id=undefined}=State) ->
    parse_location(H, State);
parse_a_values([], _, State) ->
    {ok, State};
parse_a_values([Bin | Attrs], H, S) ->
    parse_a_value(parse_kv(Bin), Attrs, H, S).

parse_a_value({ok, Key, {_, Value}, Rest}, V, H, #state{action=#occi_action{}=A}=S) ->
    try occi_action:set_attr_value(A, Key, Value) of
	#occi_action{}=A2 ->
	    parse_a_value(parse_kv(Rest), V, H, S#state{action=A2});
	{error, Err} -> {error, Err}
    catch throw:Err -> {error, Err}
    end;
parse_a_value({ok, Key, {_, Value}, Rest}, V, H, #state{entity=E}=S) ->
    try occi_entity:set_attr_value(E, Key, Value) of
	#occi_resource{}=E2 ->
	    parse_a_value(parse_kv(Rest), V, H, S#state{entity=E2});
	#occi_link{}=E2 ->
	    parse_a_value(parse_kv(Rest), V, H, S#state{entity=E2});
	{error, Err} -> {error, Err}
    catch throw:Err -> {error, Err}
    end;
parse_a_value({error, Err}, _, _, _) ->
    {error, Err};
parse_a_value(none, V, H, S) ->
    parse_a_values(V, H, S).

parse_location(Headers, #state{entity=E}=S) ->
    case orddict:find(location, Headers) of
	{ok, [undefined]} ->
	    {ok, S};
	{ok, [Location]} ->
	    try occi_uri:parse(Location) of
		#uri{}=Uri ->
		    {ok, S#state{entity=occi_entity:set_id(E, Uri)}}
	    catch
		_:Err ->
		    {error, {parse_error, Err}}
	    end;
	error ->
	    % Note: location is optional since we generate uuid if missing
	    {ok, S}
    end.

parse_kv(<<>>) ->
    none;
parse_kv(Bin) ->
    parse_key(parse_ws(Bin), <<>>).

parse_key(<< $=, Rest/bits >>, SoFar) ->
    Rest2 = parse_ws(Rest),
    parse_value(Rest2, key_to_atom(SoFar));
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

parse_string_end(<<>>, Key, Value) ->
    {ok, Key, {string, Value}, <<>>};
parse_string_end(<< $;, Rest/bits >>, Key, Value) ->
    {ok, Key, {string, Value}, Rest};
parse_string_end(_, _, _) ->
    {error, value}.

parse_number(<<>>, SoFar, Key) ->
    {ok, Key, {integer, binary_to_integer(SoFar)}, <<>>};
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
	$; ->
	    {ok, K, {integer, binary_to_integer(SoFar)}, Rest};
	$\s -> 
	    parse_number(parse_ws(Rest), SoFar, K);
	$\t ->
	    parse_number(parse_ws(Rest), SoFar, K);
	_ -> 
	    {error, value}
    end.

parse_float(<<>>, SoFar, Key) ->
    {ok, Key, {float, binary_to_float(SoFar)}, <<>>};
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
	$; -> 
	    {ok, K, {float, binary_to_float(SoFar)}, Rest};
	$\s -> 
	    parse_float(parse_ws(Rest), SoFar, K);
	$\t ->
	    parse_float(parse_ws(Rest), SoFar, K);
	_ ->
	    {error, value}
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

key_to_atom(<<"scheme">>) -> 'scheme';
key_to_atom(<<"term">>) -> 'term';
key_to_atom(<<"class">>) -> 'class';
key_to_atom(<<"title">>) -> 'title';
key_to_atom(<<"rel">>) -> 'rel';
key_to_atom(<<"location">>) -> 'location';
key_to_atom(<<"attributes">>) -> 'attributes';
key_to_atom(<<"actions">>) -> 'actions';
key_to_atom(<<"self">>) -> 'self';
key_to_atom(<<"category">>) -> 'category';
key_to_atom(Bin) -> ?attr_to_atom(Bin).
