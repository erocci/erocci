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
%%% @doc Access OCCI types static informations (mainly from modules tags)
%%%
%%% @end
%%% Created : 19 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_type).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-export([get_category/2]).
-export([get_id/1,
	 get_title/1,
	 get_relations/1,
	 get_attributes/1,
	 get_actions_spec/1,
	 get_actions/1,
	 get_entity_type/1
	]).
-export([has_property/2]).

get_category(Url, Mod) ->
    Id = get_id(Mod),
    case Id#occi_cid.class of
	kind ->
	    get_kind(Url, Mod);
	mixin ->
	    get_mixin(Url, Mod)
    end.

-spec get_kind(uri(), atom()) -> occi_kind().
get_kind(Url, Mod) ->
    [Rel] = get_relations(Mod),
    #occi_kind{id=get_id(Mod), 
	       title=get_title(Mod),
	       attributes=get_attributes(Mod),
	       rel=Rel,
	       actions=get_actions_spec(Mod),
	       location=Url}.

-spec get_mixin(uri(), atom()) -> occi_mixin().
get_mixin(Url, Mod) ->
    #occi_mixin{id=get_id(Mod),
		title=get_title(Mod),
		attributes=get_attributes(Mod),
		actions=get_actions_spec(Mod),
		location=Url}.

-spec get_actions(atom()) -> [occi_action()].
get_actions(Mod) ->
    Cid = get_id(Mod),
    Scheme = get_action_scheme(Cid),
    GenAttr = fun({K, F}) -> {K, [], F} end,
    GenAction = fun({Term, Title, Attrs}) ->
			#occi_action{id=#occi_cid{scheme=Scheme, term=Term, class=action}, 
				     title=Title, 
				     attributes=lists:map(GenAttr, Attrs)}
		end,
    Actions = get_tag(Mod, occi_action),
    lists:map(GenAction, Actions).

-spec get_id(atom()) -> occi_cid().
get_id(Mod) ->
    [Scheme] = get_tag(Mod, occi_scheme),
    [Term] = get_tag(Mod, occi_term),
    [Class] = get_tag(Mod, occi_class),
    #occi_cid{scheme=Scheme, term=Term, class=Class}.

-spec get_title(atom()) -> list().
get_title(Mod) ->
    case get_tag(Mod, occi_title) of
	[] -> {error, {einval, occi_title}};
	[Val] -> Val
    end.

-spec get_relations(atom()) -> [{atom(), atom()}].
get_relations(Mod) ->
    get_tag(Mod, occi_relation).

-spec get_attributes(atom()) -> [{atom(), list(), mfa()}].
get_attributes(Mod) ->
    get_tag(Mod, occi_attribute).

-spec get_actions_spec(atom()) -> [{tuple(), atom(), list(), list()}].
get_actions_spec(Mod) ->
    Cid = get_id(Mod),
    Scheme = get_action_scheme(Cid),
    Actions = get_tag(Mod, occi_action),
    GenAttr = fun({Term, F}) -> {Term, [], F} end,
    GenAction = fun({Term, Desc, Attrs}) -> 
			{Scheme, Term, Desc, lists:map(GenAttr, Attrs)} 
		end,
    lists:map(GenAction, Actions).

-spec get_entity_type(atom()) -> resource | link | undefined.
get_entity_type(Mod) ->
    case get_tag(Mod, occi_type) of
	[] -> undefined;
	[resource|_T] -> resource;
	[link|_T] -> link
    end.

get_tag(Mod, Name) when is_atom(Mod), is_atom(Name) ->
    Attrs = Mod:module_info(attributes),
    lists:foldl(fun({Key,[E]}, Acc) when Key == Name -> [E|Acc];
		   (_, Acc) -> Acc
		end, [], Attrs);
get_tag(Mod, Name) ->
    lager:error("Invalid value: ~p, ~p~n", [Mod, Name]),
    throw({error, einval}).

-spec get_action_scheme(occi_cid()) -> uri().
get_action_scheme(BaseId) ->
    BaseScheme = list_to_binary(lists:nth(1, string:tokens(atom_to_list(BaseId#occi_cid.scheme), "#"))),
    BaseTerm = list_to_binary(atom_to_list(BaseId#occi_cid.term)),
    << BaseScheme/binary, $/, BaseTerm/binary, $/, "action#" >>.

has_property({occi_attribute, _K, [], _F}, _Property) ->
    false;
has_property({occi_attribute, _K, [Property | _Tail], _F}, Property) ->
    true;
has_property({occi_attribute, _K, [_H | T], _F}, Property) ->
    has_property({occi_attribute, _K, T, _F}, Property).
