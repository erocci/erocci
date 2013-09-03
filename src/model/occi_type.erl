%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Access OCCI types static informations (mainly from modules tags)
%%%
%%% @end
%%% Created : 19 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_type).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-export([get_kind/1, get_mixin/1]).
-export([get_id/1,
	 get_title/1,
	 get_relations/1,
	 get_location/1,
	 get_attributes/1,
	 get_actions_spec/1,
	 get_actions/1,
	 get_entity_type/1
	]).
-export([has_property/2]).

-spec get_kind(atom()) -> occi_kind().
get_kind(Mod) ->
    [Rel] = get_relations(Mod),
    #occi_kind{id=get_id(Mod), 
	       title=get_title(Mod),
	       attributes=get_attributes(Mod),
	       rel=Rel,
	       actions=get_actions_spec(Mod),
	       location=get_location(Mod)}.

-spec get_mixin(atom()) -> occi_mixin().
get_mixin(Mod) ->
    #occi_mixin{id=get_id(Mod),
		title=get_title(Mod),
		attributes=get_attributes(Mod),
		actions=get_actions_spec(Mod),
		location=get_location(Mod)}.

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

-spec get_location(atom()) -> binary().
get_location(Mod) ->
    Base = occi_config:get(base_location),
    Term = atom_to_list(lists:nth(1, get_tag(Mod, occi_term))),
    [ Base, "/", Term].

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

get_action_scheme(BaseId) ->
    [ lists:nth(1, string:tokens(atom_to_list(BaseId#occi_cid.scheme), "#")), 
      "/", atom_to_list(BaseId#occi_cid.term), 
      "/action#" ].

has_property({occi_attribute, _K, [], _F}, _Property) ->
    false;
has_property({occi_attribute, _K, [Property | _Tail], _F}, Property) ->
    true;
has_property({occi_attribute, _K, [_H | T], _F}, Property) ->
    has_property({occi_attribute, _K, T, _F}, Property).
