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
-module(occi_mixin).

-include("occi.hrl").

%% API
-export([new/0,
	 new/1,
	 new/2, 
	 get_id/1,
	 get_class/1,
	 get_scheme/1,
	 set_scheme/2,
	 get_term/1,
	 set_term/2,
	 get_title/1,
	 set_title/2,
	 get_location/1,
	 set_location/2,
	 add_attribute/2,
	 get_attributes/1,
	 get_attr_list/1,
	 get_actions/1,
	 add_action/2,
	 get_applies/1,
	 add_applies/2,
	 get_depends/1,
	 add_depends/2,
	 is_user/1,
	 is_valid/1,
	 add_prefix/2,
	 rm_prefix/2]).

new() ->
    #occi_mixin{id=#occi_cid{class=mixin},
		actions=orddict:new(),
		attributes=orddict:new()}.

new(#occi_cid{}=Id, #uri{}=Location) ->
    #occi_mixin{id=Id, attributes=orddict:new(), actions=orddict:new(), location=Location};

new(Scheme, Term) ->
    new(#occi_cid{scheme=Scheme, term=Term, class=mixin}).

new(#occi_cid{class=mixin}=Cid) ->
    #occi_mixin{id=Cid, attributes=orddict:new(), actions=orddict:new()};
new(_) ->
    throw({error, invalid_cid}).

get_id(#occi_mixin{id=Id}) -> 
    Id.

get_class(_) -> 
    mixin.

get_scheme(#occi_mixin{id=#occi_cid{scheme=Scheme}}) -> 
    Scheme.

set_scheme(#occi_mixin{id=Id}=Mixin, Scheme) when is_binary(Scheme); is_atom(Scheme) ->
    Mixin#occi_mixin{id=Id#occi_cid{scheme=Scheme}}.

get_term(#occi_mixin{id=#occi_cid{term=Term}}) -> 
    Term.

set_term(#occi_mixin{id=Id}=Mixin, Term) when is_binary(Term); is_atom(Term) ->
    Mixin#occi_mixin{id=Id#occi_cid{term=Term}}.

get_title(#occi_mixin{title=Title}) -> 
    Title.

set_title(#occi_mixin{}=Mixin, Title) when is_binary(Title); 
					   Title =:= undefined ->
    Mixin#occi_mixin{title=Title}.

get_location(#occi_mixin{location=Uri}) -> 
    Uri.

set_location(Mixin, Uri) when is_binary(Uri)->
    set_location(Mixin, occi_uri:parse(Uri));
set_location(#occi_mixin{}=Mixin, #uri{}=Uri) -> 
    Mixin#occi_mixin{location=Uri}.

add_attribute(#occi_mixin{attributes=Attrs}=Mixin, A) -> 
    Mixin#occi_mixin{attributes=orddict:store(occi_attribute:get_id(A), A, Attrs)}.

get_attributes(#occi_mixin{attributes=Attrs}) ->
    Attrs.

get_attr_list(#occi_mixin{attributes=Attrs}) ->
    orddict:fold(fun (_Key, Value, Acc) ->
			 [Value|Acc]
		 end, [], Attrs).

get_actions(#occi_mixin{actions=Actions}) ->
    orddict:fold(fun (_Key, Action, Acc) ->
			 [Action | Acc]
		 end, [], Actions).

add_action(#occi_mixin{actions=Actions}=Mixin, Action) ->
    Mixin#occi_mixin{actions=orddict:store(occi_action:get_id(Action), Action, Actions)}.

get_applies(#occi_mixin{applies=Applies}) ->
    Applies.

add_applies(#occi_mixin{applies=Applies}=Mixin, #occi_cid{}=Cid) ->
    Mixin#occi_mixin{applies=[Cid|Applies]}.

get_depends(#occi_mixin{depends=Depends}) ->
    Depends.

add_depends(#occi_mixin{depends=Depends}=Mixin, #occi_cid{}=Cid) ->
    Mixin#occi_mixin{depends=[Cid|Depends]}.

is_user(#occi_mixin{user=User}) ->
    User.

is_valid(#occi_mixin{id=#occi_cid{class=kind}}) ->
    {false, invalid_class};
is_valid(#occi_mixin{id=#occi_cid{class=action}}) ->
    {false, invalid_class};
is_valid(#occi_mixin{}) ->
    true.

add_prefix(#occi_mixin{location=Uri}=Mixin, Prefix) when is_list(Prefix) ->
    Mixin#occi_mixin{location=occi_uri:add_prefix(Uri, Prefix)}.

rm_prefix(#occi_mixin{location=Uri}=Mixin, Prefix) when is_list(Prefix) ->
    Mixin#occi_mixin{location=occi_uri:rm_prefix(Uri, Prefix)}.
