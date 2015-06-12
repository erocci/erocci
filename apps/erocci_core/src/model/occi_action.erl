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
%%% Created : 25 Jul 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_action).

-include("occi.hrl").

-export([new/1,
	 new/2,
	 get_id/1,
	 get_class/1,
	 get_scheme/1,
	 get_term/1,
	 get_title/1,
	 set_title/2,
	 add_attribute/2,
	 get_attributes/1,
	 get_attr_list/1,
	 set_attr_value/3,
	 check/1]).

new(#occi_cid{}=Cid) ->
    #occi_action{id=Cid, attributes=orddict:new(), location=undefined}.

new(Scheme, Term) ->
    new(#occi_cid{scheme=Scheme, term=Term, class=action}).

get_id(#occi_action{id=Id}) -> 
    Id.

get_class(_) -> 
    action.

get_scheme(#occi_action{id=#occi_cid{scheme=Scheme}}) -> 
    Scheme.

get_term(#occi_action{id=#occi_cid{term=Term}}) -> 
    Term.

get_title(#occi_action{title=Title}) -> 
    Title.

set_title(#occi_action{}=Action, Title) -> 
    Action#occi_action{title=Title}.

add_attribute(#occi_action{attributes=Attrs}=Action, A) -> 
    Action#occi_action{attributes=orddict:store(occi_attribute:get_id(A), A, Attrs)}.

get_attributes(#occi_action{attributes=Attrs}) ->
    Attrs.

get_attr_list(#occi_action{attributes=Attrs}) ->
    orddict:fold(fun (_Key, Val, Acc) ->
			 [Val|Acc]
		 end, [], Attrs).

-spec set_attr_value(occi_action(), occi_attr_key(), any()) -> occi_action().
set_attr_value(#occi_action{}=A, 'occi.core.title', Val) ->
    A#occi_action{title=Val};
set_attr_value(#occi_action{}=A, 'occi.core.id', Val) ->
    A#occi_action{id=Val};
set_attr_value(#occi_action{attributes=Attrs}=A, Key, Val) when is_binary(Key); 
								is_atom(Key) ->
    case orddict:is_key(Key, Attrs) of
	true ->
	    Attr = orddict:fetch(Key, Attrs),
	    A#occi_action{attributes=orddict:store(Key, occi_attribute:set_value(Attr, Val), Attrs)};
	false ->
	    {error, {undefined_attribute, Key}}
    end.

-spec check(occi_action()) -> ok | {error, term()}.
check(#occi_action{attributes=Attrs}) ->
    orddict:fold(fun (_Key, Attr, Acc) ->
			 case occi_attribute:check(Attr) of
			     ok -> Acc;
			     error -> {error, badarg}
			 end
		 end, ok, Attrs).
