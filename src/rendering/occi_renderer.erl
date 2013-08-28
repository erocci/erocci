%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Access OCCI types static informations (mainly from modules tags)
%%%
%%% @end
%%% Created : 19 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_renderer).
-compile([{parse_transform, lager_transform}]).

-include("occi_renderer.hrl").

-export([get_id/1,
	 get_title/1,
	 get_class/1,
	 get_relations/1,
	 get_location/1,
	 get_attributes/1,
	 get_actions_spec/1,
	 get_actions/1
	]).

-spec get_id(atom()) -> {occi_cid, atom(), atom()}.
get_id(Mod) ->
    [Scheme] = get_tag(Mod, occi_scheme),
    [Term] = get_tag(Mod, occi_term),
    {occi_cid, Scheme, Term}.

-spec get_title(atom()) -> {occi_title, list()}.
get_title(Mod) ->
    case get_tag(Mod, occi_title) of
	[] -> {error, {einval, occi_title}};
	[Val] -> {occi_title, Val}
    end.

-spec get_class(atom()) -> {occi_class, atom()}.
get_class(Mod) ->
    case get_tag(Mod, occi_class) of
	[kind] -> {occi_class, occi_kind};
	[mixin] -> {occi_class, occi_mixin};
	[action] -> {occi_class, occi_action};
	_ -> {error, {einval, occi_class}}
    end.

-spec get_relations(atom()) -> {occi_relations, [{occi_relation, atom(), atom()}]}.
get_relations(Mod) ->
    F = fun({Scheme, Term}) -> {occi_relation, Scheme, Term} end,
    {occi_relations, lists:map(F ,get_tag(Mod, occi_relation))}.

-spec get_location(atom()) -> {occi_location, binary()}.
get_location(Mod) ->
    Base = occi_config:get(base_location),
    Term = ?ATOM_TO_BINARY(lists:nth(1, get_tag(Mod, occi_term))),
    {occi_location, <<Base/binary, "/", Term/binary>>}.

-spec get_attributes(atom()) -> {occi_attributes, [{occi_attribute, atom(), list(), mfa()}]}.
get_attributes(Mod) ->
    GenAttr = fun({K, P, F}) -> {occi_attribute, K, P, F} end,
    {occi_attributes, lists:map(GenAttr, get_tag(Mod, occi_attribute))}.

-spec get_actions_spec(atom()) -> {occi_actions_spec, [{occi_action_spec, tuple(), atom(), list(), list()}]}.
get_actions_spec(Mod) ->
    CatId = get_id(Mod),
    Actions = get_tag(Mod, occi_action),
    GenAttr = fun({Term, F}) -> {occi_attribute, Term, F} end,
    GenAction = fun({Name, Desc, Attrs}) -> 
			{occi_action_spec, CatId, Name, Desc, lists:map(GenAttr, Attrs)} 
		end,
    {occi_actions_spec, lists:map(GenAction, Actions)}.

-spec get_actions(atom()) -> [{occi_action, atom(), atom(), binary(), list()}].
get_actions(Mod) ->
    {occi_cid, BaseScheme, BaseTerm} = get_id(Mod),
    Scheme = [ ?ATOM_TO_BINARY(BaseScheme), "/", ?ATOM_TO_BINARY(BaseTerm), "/action#" ],
    GenAttr = fun({K, F}) -> {occi_attribute, K, F} end,		      
    GenAction = fun({Term, Title, Attrs}) ->
			{occi_action, Scheme, Term, Title, lists:map(GenAttr, Attrs)}
		end,
    Actions = get_tag(Mod, occi_action),
    lists:map(GenAction, Actions).

get_tag(Mod, Name) when is_atom(Mod), is_atom(Name) ->
    Attrs = Mod:module_info(attributes),
    lists:foldl(fun({Key,[E]}, Acc) when Key == Name -> [E|Acc];
		   (_, Acc) -> Acc
		end, [], Attrs);
get_tag(Mod, Name) ->
    lager:error("Invalid value: ~p, ~p~n", [Mod, Name]),
    throw({error, einval}).
