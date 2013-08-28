%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_renderer_plain).
-compile({parse_transform, lager_transform}).

-include("occi_renderer.hrl").

%% API
-export([render/1]).

%%%===================================================================
%%% API
%%%===================================================================
render(Mod) when is_atom(Mod) ->
    render({occi_type, occi_renderer:get_id(Mod), Mod, undefined});

render({occi_type, Id, Mod, _}) ->
    {occi_class, Class} = occi_renderer:get_class(Mod),
    [ render(Id),
      render(occi_renderer:get_title(Mod)),
      render({Class, Mod}),
      <<"\n">> ];
render({occi_kind, Mod}) ->
    [ render({occi_class, <<"kind">>}),
      render(occi_renderer:get_relations(Mod)),
      render(occi_renderer:get_location(Mod)),
      render(occi_renderer:get_attributes(Mod)),
      render(occi_renderer:get_actions_spec(Mod)),
      <<"\n">>
    ];
render({occi_mixin, Mod}) ->
    [ render({occi_class, <<"mixin">>}),
      render(occi_renderer:get_relations(Mod)),
      render(occi_renderer:get_location(Mod)),
      render(occi_renderer:get_attributes(Mod)),
      render(occi_renderer:get_actions_spec(Mod)),
      <<"\n">>
    ];
render({occi_action, Scheme, Term, Title, Attributes}) ->
    [ render({occi_cid, Scheme, Term}),
      render({occi_title, Title}),
      render({occi_class, <<"action">>}),
      render({occi_attributes, Attributes}),
      <<"\n">>
    ];

render({occi_cid, Scheme, Term}) when is_atom(Scheme) ->
    render({occi_cid, ?ATOM_TO_BINARY(Scheme), Term});
render({occi_cid, Scheme, Term}) ->
    [ <<"Category: ">>, ?ATOM_TO_BINARY(Term), <<"">>,
      <<";\n\tscheme=\"">>, Scheme, <<"\"">> ];

render({occi_class, E}) ->
    [ <<";\n\tclass=\"">>, E, <<"\"">> ];

render({occi_title, E}) ->
    [ <<";\n\ttitle=\"">>, E, <<"\"">> ];

render({occi_relations, E}) when is_list(E) ->
    [ <<";\n\trel=\"">>, render_ssi(E, fun render/1), <<"\"">> ];

render({occi_location, E}) ->
    [ <<";\n\tlocation=\"">>, E, <<"\"">> ];

render({occi_attributes, L}) ->
    [ <<";\n\tattributes=\"">>, render_ssi(L, fun render/1), <<"\"">> ];

render({occi_actions_spec, L}) ->
    [ <<";\n\tactions=\"">>, render_ssi(L, fun render/1), <<"\"">> ];

render({occi_relation, Scheme, Term}) ->
    [ ?ATOM_TO_BINARY(Scheme), "/", ?ATOM_TO_BINARY(Term) ];

render({occi_attribute, K, [], _F}) ->
    ?ATOM_TO_BINARY(K);
render({occi_attribute, K, L, _F}) ->
    [ ?ATOM_TO_BINARY(K), <<"{">>, render_attr_properties(L), <<"}">> ];
render({occi_attribute, K, _F}) ->
    ?ATOM_TO_BINARY(K);

render({occi_action_spec, {occi_cid, Scheme, Term}, Name, _Desc, _Attrs}) ->
    [ ?ATOM_TO_BINARY(Scheme), <<"/">>,
      ?ATOM_TO_BINARY(Term), <<"/action#">>,
      ?ATOM_TO_BINARY(Name) ];

render({_, undefined}) ->
    <<>>;

render(O) ->
    lager:error("Invalid value: ~p~n", [O]),
    throw({error, {occi_syntax, "invalid value"}}).

% Render space-separated items
render_ssi(L, F) ->
    render_ssi(L, F, []).

render_ssi([], _, Acc) ->
    lists:reverse(Acc);
render_ssi([E], F, Acc) ->
    lists:reverse([F(E) | Acc]);
render_ssi([H|T], F, Acc) ->
    render_ssi(T, F, [<<" ">>, F(H) | Acc]).

render_attr_properties([]) ->
    <<>>;
render_attr_properties([H | T]) when T == [] ->
    ?ATOM_TO_BINARY(H);
render_attr_properties([ H | T ]) ->
    [ ?ATOM_TO_BINARY(H), <<",">>, render_attr_properties(T) ].
