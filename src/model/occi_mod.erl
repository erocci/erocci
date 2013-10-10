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
-module(occi_mod).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-export([get_category/1,
	 get_kind/1,
	 get_mixin/1]).
-export([get_id/1,
	 get_title/1,
	 get_relations/1,
	 get_attributes/1,
	 get_actions/1,
	 get_entity_type/1
	]).

-spec get_category(atom()) -> occi_category().
get_category(Mod) ->
    Id = get_id(Mod),
    case Id#occi_cid.class of
	kind ->
	    get_kind(Mod);
	mixin -> 
	    get_mixin(Mod)
    end.

-spec get_kind(atom()) -> occi_kind().
get_kind(Mod) ->
    #occi_kind{id=get_id(Mod), 
	       title=get_title(Mod),
	       attributes=get_attributes(Mod),
	       rel=lists:nth(1, get_relations(Mod)),
	       actions=get_actions(Mod)}.

-spec get_mixin(atom()) -> occi_mixin().
get_mixin(Mod) ->
    #occi_mixin{id=get_id(Mod),
		title=get_title(Mod),
		attributes=get_attributes(Mod),
		actions=get_actions(Mod)}.

-spec get_actions(atom()) -> [occi_action_spec()].
get_actions(Mod) ->
    Actions = get_tag(Mod, occi_action_spec),
    lists:map(fun gen_action/1, Actions).

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
    lists:map(fun gen_relation/1, get_tag(Mod, occi_relation)).

-spec get_attributes(atom()) -> [{atom(), list(), mfa()}].
get_attributes(Mod) ->
    lists:map(fun gen_attr_spec/1, get_tag(Mod, occi_attribute)).

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

%%%
%%% Functions for transforming module tags into corresponding records
%%%
gen_attr_spec({Id, Type}) ->
    #occi_attr_spec{id=Id, type=Type};
gen_attr_spec({Id, Type, Properties}) ->
    #occi_attr_spec{id=Id, type=Type, properties=Properties}.

gen_action({Scheme, Term, Title, Attributes}) ->
    #occi_action_spec{id=#occi_cid{scheme=Scheme, term=Term, class=action}, 
		      title=Title, 
		      attributes=lists:map(fun gen_attr_spec/1, Attributes)}.

gen_relation({Scheme, Term}) ->
    #occi_cid{scheme=Scheme, term=Term}.

