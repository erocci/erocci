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
-module(occi_kind).

-include("occi.hrl").

-export([new/2,
	 get_parent/1,
	 set_parent/2,
	 get_actions/1,
	 add_action/2,
	 get_id/1,
	 get_class/1,
	 get_scheme/1,
	 get_term/1,
	 get_title/1,
	 set_title/2,
	 add_attribute/2,
	 get_attributes/1,
	 get_attr_list/1,
	 get_location/1]).

new(Scheme, Term) ->
    #occi_kind{id=#occi_cid{scheme=Scheme, term=Term, class=kind},
	       actions=orddict:new(),
	       attributes=orddict:new()}.

get_id(#occi_kind{id=Id}) -> 
    Id.

get_class(_) -> 
    kind.

get_scheme(#occi_kind{id=#occi_cid{scheme=Scheme}}) -> 
    Scheme.

get_term(#occi_kind{id=#occi_cid{term=Term}}) -> 
    Term.

get_title(#occi_kind{title=Title}) -> 
    Title.

set_title(#occi_kind{}=Kind, Title) -> 
    Kind#occi_kind{title=Title}.

add_attribute(#occi_kind{attributes=Attrs}=Kind, A) -> 
    Kind#occi_kind{attributes=orddict:store(occi_attribute:get_id(A), A, Attrs)}.

get_attributes(#occi_kind{attributes=Attrs}) ->
    Attrs.

get_attr_list(#occi_kind{attributes=Attrs}) ->
    orddict:fold(fun (_Key, Value, Acc) ->
			 [Value|Acc]
		 end, [], Attrs).

get_actions(#occi_kind{actions=Actions}) ->
    orddict:fold(fun (_Key, Action, Acc) ->
			 [Action | Acc]
		 end, [], Actions).

add_action(#occi_kind{actions=Actions}=Kind, Action) ->
    Kind#occi_kind{actions=orddict:store(occi_action:get_id(Action), Action, Actions)}.

get_parent(#occi_kind{parent=Parent}) ->
    Parent.

set_parent(#occi_kind{}=Kind, #occi_cid{}=Cid) ->
    Kind#occi_kind{parent=Cid}.

get_location(#occi_kind{location=Uri}) ->
    Uri.
