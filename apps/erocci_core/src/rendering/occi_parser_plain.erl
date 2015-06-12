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
%%% Created : 5 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_parser_plain).

-include("occi_parser_text.hrl").

%% API
-export([parse_action/3,
	 parse_entity/3,
	 parse_user_mixin/2,
	 parse_collection/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_action(Data, _Env, Action) ->
    case parse_header(Data, orddict:new()) of
	{error, Err} ->
	    {error, Err};
	{ok, Headers} ->
	    occi_parser_text:parse_action(Headers, #state{action=Action})
    end.    

parse_entity(Data, Env, #occi_resource{}=Res) ->
    case parse_header(Data, orddict:new()) of
	{error, Err} ->
	    {error, Err};
	{ok, Headers} ->
	    {Location, _} = cowboy_req:header(<<"location">>, Env),
	    Headers2 = add_header_value('location', Location, Headers),
	    occi_parser_text:parse_entity(Headers2, #state{entity=Res})
    end;

parse_entity(Data, Env, #occi_link{}=Link) ->
    case parse_header(Data, orddict:new()) of
	{error, Err} ->
	    {error, Err};
	{ok, Headers} ->
	    {Location, _} = cowboy_req:header(<<"location">>, Env),
	    Headers2 = add_header_value('location', Location, Headers),
	    occi_parser_text:parse_entity(Headers2, #state{entity=Link})
    end;

parse_entity(Data, _Env, #occi_entity{id=Id}) ->
    case parse_header(Data, orddict:new()) of
	{error, Err} ->
	    {error, Err};
	{ok, Headers} ->
	    occi_parser_text:parse_entity(Headers, #state{entity_id=Id})
    end.

parse_user_mixin(Data, _Env) ->
    case parse_header(Data, orddict:new()) of
	{error, Err} ->
	    {error, Err};
	{ok, Headers} ->
	    occi_parser_text:parse_user_mixin(Headers, #state{mixin=occi_mixin:new(#occi_cid{class=mixin})})
    end.

parse_collection(Data, _Env) ->
    case parse_header(Data, orddict:new()) of
	{error, Err} ->
	    {error, Err};
	{ok, Headers} ->
	    occi_parser_text:parse_collection(Headers, #state{})
    end.

%%%
%%% Private
%%% Largely inspired by cowboy_protocol
%%%
parse_header(<<>>, Headers) ->
    {ok, reverse(Headers)};
parse_header(<< $\r, Rest/bits >>, Headers) ->
    parse_header(Rest, Headers);
parse_header(<< $\n, _Rest/bits >>, Headers) ->
    {ok, reverse(Headers)};
parse_header(Buffer, H) ->
    case match_colon(Buffer, 0) of
	nomatch ->
	    parse_header(Buffer, H);
	_ ->
	    parse_hd_name(Buffer, H, <<>>)
    end.

reverse(H) ->
    orddict:fold(fun (Key, Values, Acc) ->
			 orddict:store(Key, lists:reverse(Values), Acc)
		 end, orddict:new(), H).

match_colon(<< $:, _/bits >>, N) ->
    N;
match_colon(<< _, Rest/bits >>, N) ->
    match_colon(Rest, N + 1);
match_colon(_, _) ->
    nomatch.

parse_hd_name(<< C, Rest/bits >>, H, SoFar) ->
    case C of
	$: -> parse_hd_before_value(Rest, H, SoFar);
	$\s -> parse_hd_name_ws(Rest, H, SoFar);
	$\t -> parse_hd_name_ws(Rest, H, SoFar);
	$A -> parse_hd_name(Rest, H, << SoFar/binary, $a >>);
	$B -> parse_hd_name(Rest, H, << SoFar/binary, $b >>);
	$C -> parse_hd_name(Rest, H, << SoFar/binary, $c >>);
	$D -> parse_hd_name(Rest, H, << SoFar/binary, $d >>);
	$E -> parse_hd_name(Rest, H, << SoFar/binary, $e >>);
	$F -> parse_hd_name(Rest, H, << SoFar/binary, $f >>);
	$G -> parse_hd_name(Rest, H, << SoFar/binary, $g >>);
	$H -> parse_hd_name(Rest, H, << SoFar/binary, $h >>);
	$I -> parse_hd_name(Rest, H, << SoFar/binary, $i >>);
	$J -> parse_hd_name(Rest, H, << SoFar/binary, $j >>);
	$K -> parse_hd_name(Rest, H, << SoFar/binary, $k >>);
	$L -> parse_hd_name(Rest, H, << SoFar/binary, $l >>);
	$M -> parse_hd_name(Rest, H, << SoFar/binary, $m >>);
	$N -> parse_hd_name(Rest, H, << SoFar/binary, $n >>);
	$O -> parse_hd_name(Rest, H, << SoFar/binary, $o >>);
	$P -> parse_hd_name(Rest, H, << SoFar/binary, $p >>);
	$Q -> parse_hd_name(Rest, H, << SoFar/binary, $q >>);
	$R -> parse_hd_name(Rest, H, << SoFar/binary, $r >>);
	$S -> parse_hd_name(Rest, H, << SoFar/binary, $s >>);
	$T -> parse_hd_name(Rest, H, << SoFar/binary, $t >>);
	$U -> parse_hd_name(Rest, H, << SoFar/binary, $u >>);
	$V -> parse_hd_name(Rest, H, << SoFar/binary, $v >>);
	$W -> parse_hd_name(Rest, H, << SoFar/binary, $w >>);
	$X -> parse_hd_name(Rest, H, << SoFar/binary, $x >>);
	$Y -> parse_hd_name(Rest, H, << SoFar/binary, $y >>);
	$Z -> parse_hd_name(Rest, H, << SoFar/binary, $z >>);
	C -> parse_hd_name(Rest, H, << SoFar/binary, C >>)
    end.

parse_hd_name_ws(<< C, Rest/bits >>, H, Name) ->
    case C of
	$\s -> parse_hd_name_ws(Rest, H, Name);
	$\t -> parse_hd_name_ws(Rest, H, Name);
	$: -> parse_hd_before_value(Rest, H, Name)
    end.

parse_hd_before_value(<< $\s, Rest/bits >>, H, N) ->
    parse_hd_before_value(Rest, H, N);
parse_hd_before_value(<< $\t, Rest/bits >>, H, N) ->
    parse_hd_before_value(Rest, H, N);
parse_hd_before_value(Buffer, H, N) ->
    parse_hd_value(Buffer, H, N, <<>>).

parse_hd_value(<< $\r, Rest/bits >>, Headers, Name, SoFar) ->
    parse_hd_value(Rest, Headers, Name, SoFar);
parse_hd_value(<< $\n, C, Rest/bits >>, Headers, Name, SoFar) 
  when C =:= $\s; C =:= $\t ->
    parse_hd_value(Rest, Headers, Name, SoFar);
parse_hd_value(<< $\n, Rest/bits >>, Headers, Name, SoFar) ->
    parse_header(Rest, add_header_value(Name, SoFar, Headers));
parse_hd_value(<< C, Rest/bits >>, H, N, SoFar) ->
    parse_hd_value(Rest, H, N, << SoFar/binary, C >>);
parse_hd_value(<<>>, _H, _N, _SoFar) ->
    {error, eof}.

add_header_value(Name, Value, Acc) when is_binary(Name) ->
    add_header_value(?hdr_to_atom(Name), Value, Acc);
add_header_value(Name, Value, Acc) ->
    Values = case orddict:find(Name, Acc) of
		 {ok, V} -> V;
		 error -> []
	     end,
    orddict:store(Name, [Value | Values], Acc).
