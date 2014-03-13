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
	    parse_c_term(parse_ws(Bin), V, H, S, <<>>);
	_ ->
	    {error, {parse_error, bad_term}}
    end.

parse_c_term(<< $;, Rest/bits >>, V, H, S, SoFar)->
    Term = list_to_atom(binary_to_list(SoFar)),
    parse_c_kv(Rest, Term, V, H, S);
parse_c_term(<< C, Rest/bits >>, V, H, S, SoFar) ->
    parse_c_term(Rest, V, H, S, << SoFar/binary, C >>);
parse_c_term(_, _, _, _, _) ->
    {error, {parse_error, invalid_category}}.

parse_c_kv(Bin, Term, [], H, #state{mixin=#occi_mixin{}}=S) ->
    case parse_kv(Bin) of
	{ok, Dict} ->
	    make_mixin(dict:store(term, {atom, Term}, Dict), H, S);
	{error, Err} ->
	    {error, Err}
    end;
parse_c_kv(_, _, _V, _, #state{mixin=#occi_mixin{}}) ->
    {error, invalid_mixin};
parse_c_kv(Bin, Term, [], H, #state{action=#occi_action{}}=S) ->
    case parse_kv(Bin) of
	{ok, Dict} ->
	    make_action(dict:store(term, {atom, Term}, Dict), H, S);
	{error, Err} ->
	    {error, Err}
    end;
parse_c_kv(_, _, _V, _, #state{action=#occi_action{}}) ->
    {error, invalid_action};
parse_c_kv(Bin, Term, V, H, S) ->
    case parse_kv(Bin) of
	{ok, Dict} ->
	    add_category(dict:store(term, {atom, Term}, Dict), V, H, S);
	{error, Err} ->
	    {error, Err}
    end.

make_action(Dict, H, S) ->
    case dict:find(<<"class">>, Dict) of
	{ok, {string, <<"action">>}} ->
	    case make_cid(Dict, action) of
		#occi_cid{}=Cid ->
		    try occi_category_mgr:get(Cid) of
			#occi_action{}=A ->
			    parse_attributes(H, S#state{action=A});
			_ ->
			    {error, invalid_category}
		    catch
			throw:Err ->
			    {error, Err}
		    end;
		{error, Err} ->
		    {error, Err}
	    end;
	{ok, Val} -> 
	    {error, {einval, Val}};
	error ->
	    {error, missing_class}
    end.

make_mixin(Dict, _, #state{mixin=#occi_mixin{id=#occi_cid{class=Cls}}=Mixin}=S) ->
    case make_cid(Dict, Cls) of
	#occi_cid{}=Cid ->
	    case dict:find(<<"location">>, Dict) of
		{ok, {string, Val}} ->
		    try occi_uri:parse(Val) of
			#uri{}=Uri ->
			    {ok, S#state{mixin=Mixin#occi_mixin{id=Cid, location=Uri}}}
		    catch
			throw:Err ->
			    {error, Err}
		    end;
		{ok, Val} ->
		    {error, {einval, Val}};
		error ->
		    {error, missing_location}
	    end;
	{error, Err} ->
	    {error, Err}
    end.

add_category(Dict, V, H, S) ->
    case dict:find(<<"class">>, Dict) of
	{ok, {string, Bin}} ->
	    case make_cid(Dict, to_atom(Bin)) of
		#occi_cid{}=Cid ->
		    try occi_category_mgr:get(Cid) of
			#occi_kind{}=Kind ->
			    new_entity(Kind, V, H, S);
			#occi_mixin{}=Mixin ->
			    add_mixin(Mixin, V, H, S);
			_ ->
			    {error, {einval, Cid}}
		    catch
			throw:Err ->
			    {error, Err}
		    end;
		{ok, Val} ->
		    {error, {einval, Val}};
		{error, Err} ->
		    {error, Err}
	    end;

	{error, Err} ->
	    {error, Err}
    end.

make_cid(Dict, Class) ->
    case dict:find(term, Dict) of
	{ok, {atom, Term}} ->
	    case dict:find(<<"scheme">>, Dict) of
		{ok, {string, Scheme}} ->
		    #occi_cid{scheme=to_atom(Scheme), term=Term, class=Class};
		{ok, Val} ->
		    {error, {einval, Val}};
		{error, Err} -> 
		    {error, Err}
	    end;
	{ok, Val} ->
	    {error, {einval, Val}};
	{error, Err} -> {error, Err}
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
    parse_a_value(Bin, Attrs, H, S).

parse_a_value(Bin, V, H, S) ->
    case parse_kv(Bin) of
	{ok, Dict} ->
	    case dict:to_list(Dict) of
		[{Key, {_Type, Value}}] ->
		    add_attr(list_to_atom(binary_to_list(Key)), Value, V, H, S);
		_ ->
		    {error, invalid_attribute}
	    end;
	{error, Err} ->
	    {error, Err}
    end.

add_attr(Key, Value, Attrs, H, #state{action=#occi_action{}=A}=S) ->
    try occi_action:set_attr_value(A, Key, Value) of
	#occi_action{}=A2 ->
	    parse_a_values(Attrs, H, S#state{action=A2});
	{error, Err} ->
	    {error, {parse_error, Err}}
    catch
	_:Err ->
	    {error, {parse_error, Err}}
    end;
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
	    % Note: location is optional since we generate uuid if missing
	    {ok, S}
    end.

parse_kv(Bin) ->
    parse_key(parse_ws(Bin), <<>>, dict:new()).

parse_key(<< $=, Rest/bits >>, SoFar, D) ->
    Rest2 = parse_ws(Rest),
    parse_value(Rest2, SoFar, D);
parse_key(<< C, Rest/bits >>, SoFar, D) ->
    case C of
	$a -> parse_key(Rest, << SoFar/binary, $a >>, D);
	$b -> parse_key(Rest, << SoFar/binary, $b >>, D);
	$c -> parse_key(Rest, << SoFar/binary, $c >>, D);
	$d -> parse_key(Rest, << SoFar/binary, $d >>, D);
	$e -> parse_key(Rest, << SoFar/binary, $e >>, D);
	$f -> parse_key(Rest, << SoFar/binary, $f >>, D);
	$g -> parse_key(Rest, << SoFar/binary, $g >>, D);
	$h -> parse_key(Rest, << SoFar/binary, $h >>, D);
	$i -> parse_key(Rest, << SoFar/binary, $i >>, D);
	$j -> parse_key(Rest, << SoFar/binary, $j >>, D);
	$k -> parse_key(Rest, << SoFar/binary, $k >>, D);
	$l -> parse_key(Rest, << SoFar/binary, $l >>, D);
	$m -> parse_key(Rest, << SoFar/binary, $m >>, D);
	$n -> parse_key(Rest, << SoFar/binary, $n >>, D);
	$o -> parse_key(Rest, << SoFar/binary, $o >>, D);
	$p -> parse_key(Rest, << SoFar/binary, $p >>, D);
	$q -> parse_key(Rest, << SoFar/binary, $q >>, D);
	$r -> parse_key(Rest, << SoFar/binary, $r >>, D);
	$s -> parse_key(Rest, << SoFar/binary, $s >>, D);
	$t -> parse_key(Rest, << SoFar/binary, $t >>, D);
	$u -> parse_key(Rest, << SoFar/binary, $u >>, D);
	$v -> parse_key(Rest, << SoFar/binary, $v >>, D);
	$w -> parse_key(Rest, << SoFar/binary, $w >>, D);
	$x -> parse_key(Rest, << SoFar/binary, $x >>, D);
	$y -> parse_key(Rest, << SoFar/binary, $y >>, D);
	$z -> parse_key(Rest, << SoFar/binary, $z >>, D);
	$0 -> parse_key(Rest, << SoFar/binary, $0 >>, D);
	$1 -> parse_key(Rest, << SoFar/binary, $1 >>, D);
	$2 -> parse_key(Rest, << SoFar/binary, $2 >>, D);
	$3 -> parse_key(Rest, << SoFar/binary, $3 >>, D);
	$4 -> parse_key(Rest, << SoFar/binary, $4 >>, D);
	$5 -> parse_key(Rest, << SoFar/binary, $5 >>, D);
	$6 -> parse_key(Rest, << SoFar/binary, $6 >>, D);
	$7 -> parse_key(Rest, << SoFar/binary, $7 >>, D);
	$8 -> parse_key(Rest, << SoFar/binary, $8 >>, D);
	$9 -> parse_key(Rest, << SoFar/binary, $9 >>, D);
	$. -> parse_key(Rest, << SoFar/binary, $. >>, D);
	C ->
	    {error, {parse_error, invalid_key}}
    end.

parse_value(<< $", Rest/bits >>, K, D) ->
    parse_string(Rest, K, <<>>, D);
parse_value(<< C, Rest/bits >>, K, D) ->
    case C of
	$0 -> parse_number(<< Rest/binary >>, << $0 >>, K, D);
	$1 -> parse_number(<< Rest/binary >>, << $1 >>, K, D);
	$2 -> parse_number(<< Rest/binary >>, << $2 >>, K, D);
	$3 -> parse_number(<< Rest/binary >>, << $3 >>, K, D);
	$4 -> parse_number(<< Rest/binary >>, << $4 >>, K, D);
	$5 -> parse_number(<< Rest/binary >>, << $5 >>, K, D);
	$6 -> parse_number(<< Rest/binary >>, << $6 >>, K, D);
	$7 -> parse_number(<< Rest/binary >>, << $7 >>, K, D);
	$8 -> parse_number(<< Rest/binary >>, << $8 >>, K, D);
	$9 -> parse_number(<< Rest/binary >>, << $9 >>, K, D);
	$+ -> parse_number(<< Rest/binary >>, << $+ >>, K, D);
	$- -> parse_number(<< Rest/binary >>, << $- >>, K, D);
	$. -> parse_float(<< Rest/binary >>, << $. >>, K, D);
	_ -> 
	    {error, {parse_error, value}}
    end.

parse_string(<< $", Rest/bits >>, K, V, D) ->
    Rest2 = parse_ws(Rest),
    parse_string_end(Rest2, K, V, D);
parse_string(<< C, Rest/bits >>, K, SoFar, D) ->
    parse_string(Rest, K, << SoFar/binary, C >>, D).

parse_string_end(<<>>, Key, Value, Dict) ->
    {ok, dict:store(Key, {string, Value}, Dict)};
parse_string_end(<< $;, Rest/bits >>, Key, Value, Dict) ->
    parse_key(parse_ws(Rest), <<>>, dict:store(Key, {string, Value}, Dict));
parse_string_end(_, _, _, _) ->
    {error, {parse_error, value}}.

parse_number(<<>>, SoFar, Key, Dict) ->
    {ok, dict:store(Key, {integer, binary_to_integer(SoFar)}, Dict)};
parse_number(<< C, Rest/bits >>, SoFar, K, D) ->
    case C of
	$0 -> parse_number(<< Rest/binary >>, << SoFar/binary, $0 >>, K, D);
	$1 -> parse_number(<< Rest/binary >>, << SoFar/binary, $1 >>, K, D);
	$2 -> parse_number(<< Rest/binary >>, << SoFar/binary, $2 >>, K, D);
	$3 -> parse_number(<< Rest/binary >>, << SoFar/binary, $3 >>, K, D);
	$4 -> parse_number(<< Rest/binary >>, << SoFar/binary, $4 >>, K, D);
	$5 -> parse_number(<< Rest/binary >>, << SoFar/binary, $5 >>, K, D);
	$6 -> parse_number(<< Rest/binary >>, << SoFar/binary, $6 >>, K, D);
	$7 -> parse_number(<< Rest/binary >>, << SoFar/binary, $7 >>, K, D);
	$8 -> parse_number(<< Rest/binary >>, << SoFar/binary, $8 >>, K, D);
	$9 -> parse_number(<< Rest/binary >>, << SoFar/binary, $9 >>, K, D);
	$. -> parse_float(<< Rest/binary >>, << $. >>, K, D);
	$; ->
	    parse_key(parse_ws(Rest), <<>>, dict:store(K, {integer, binary_to_integer(SoFar)}, D));
	$\s -> 
	    parse_number(parse_ws(Rest), SoFar, K, D);
	$\t ->
	    parse_number(parse_ws(Rest), SoFar, K, D);
	_ -> 
	    {error, {parse_error, value}}
    end.

parse_float(<<>>, SoFar, Key, Dict) ->
    {ok, dict:store(Key, {float, binary_to_float(SoFar)}, Dict)};
parse_float(<< C, Rest/bits >>, SoFar, K, D) ->
    case C of
	$0 -> parse_float(<< Rest/binary >>, << SoFar/binary, $0 >>, K, D);
	$1 -> parse_float(<< Rest/binary >>, << SoFar/binary, $1 >>, K, D);
	$2 -> parse_float(<< Rest/binary >>, << SoFar/binary, $2 >>, K, D);
	$3 -> parse_float(<< Rest/binary >>, << SoFar/binary, $3 >>, K, D);
	$4 -> parse_float(<< Rest/binary >>, << SoFar/binary, $4 >>, K, D);
	$5 -> parse_float(<< Rest/binary >>, << SoFar/binary, $5 >>, K, D);
	$6 -> parse_float(<< Rest/binary >>, << SoFar/binary, $6 >>, K, D);
	$7 -> parse_float(<< Rest/binary >>, << SoFar/binary, $7 >>, K, D);
	$8 -> parse_float(<< Rest/binary >>, << SoFar/binary, $8 >>, K, D);
	$9 -> parse_float(<< Rest/binary >>, << SoFar/binary, $9 >>, K, D);
	$; -> 
	    parse_key(parse_ws(Rest), <<>>, dict:store(K, {float, binary_to_float(SoFar)}, D));
	$\s -> 
	    parse_float(parse_ws(Rest), SoFar, K, D);
	$\t ->
	    parse_float(parse_ws(Rest), SoFar, K, D);
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

to_atom(Bin) when is_binary(Bin) ->
    list_to_atom(binary_to_list(Bin)).
