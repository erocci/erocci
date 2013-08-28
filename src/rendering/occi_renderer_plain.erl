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

%% API
-export([render_plain/1,
	 render_occi/1,
	 render_uri_list/1]).

%%%===================================================================
%%% API
%%%===================================================================
render_plain(Types) ->
    lists:map(fun(Type) -> [ <<"Categories: ">>, render(Type, '\n')] end, Types).

render_occi(Types) ->
    render_occi(Types, []).

render_uri_list(Types) ->
    RenderLocation = fun({occi_type, _, Mod, _}) -> 
			     {occi_location, Location} = occi_renderer:get_location(Mod),
			     [ Location, <<"\n">> ];
			({occi_action,_,_,_,_}) -> []
		     end,
    lists:map(RenderLocation, Types).

render_occi([], Acc) ->
    lists:reverse(Acc);
render_occi([Type | Tail], Acc) when Tail == [] ->
    [ render(Type, ' ') | Acc ];
render_occi([Type | Tail], Acc) ->
    [ [ render(Type, ' '), <<", ">> ] | render_occi(Tail, Acc) ].

render(Mod, Sep) when is_atom(Mod) ->
    render({occi_type, occi_renderer:get_id(Mod), Mod, undefined}, Sep);

render({occi_type, Id, Mod, _}, Sep) ->
    {occi_class, Class} = occi_renderer:get_class(Mod),
    [ render(Id, Sep),
      render(occi_renderer:get_title(Mod), Sep),
      render({Class, Mod}, Sep),
      render_sep(Sep) ];
render({occi_kind, Mod}, Sep) ->
    [ render({occi_class, <<"kind">>}, Sep),
      render(occi_renderer:get_relations(Mod), Sep),
      render(occi_renderer:get_location(Mod), Sep),
      render(occi_renderer:get_attributes(Mod), Sep),
      render(occi_renderer:get_actions_spec(Mod), Sep),
      render_sep(Sep)
    ];
render({occi_mixin, Mod}, Sep) ->
    [ render({occi_class, <<"mixin">>}, Sep),
      render(occi_renderer:get_relations(Mod), Sep),
      render(occi_renderer:get_location(Mod), Sep),
      render(occi_renderer:get_attributes(Mod), Sep),
      render(occi_renderer:get_actions_spec(Mod), Sep),
      render_sep(Sep)
    ];
render({occi_action, Scheme, Term, Title, Attributes}, Sep) ->
    [ render({occi_cid, Scheme, Term}, Sep),
      render({occi_title, Title}, Sep),
      render({occi_class, <<"action">>}, Sep),
      render({occi_attributes, Attributes}, Sep),
      render_sep(Sep)
    ];

render({occi_cid, Scheme, Term}, Sep) when is_atom(Scheme) ->
    render({occi_cid, atom_to_list(Scheme), Term}, Sep);
render({occi_cid, Scheme, Term}, Sep) ->
    [ atom_to_list(Term), <<";">>, render_sep_indent(Sep), <<"scheme=\"">>, Scheme, <<"\"">> ];

render({occi_class, E}, Sep) ->
    [ ";", render_sep_indent(Sep), <<"class=\"">>, E, <<"\"">> ];

render({occi_title, E}, Sep) ->
    [ <<";">>, render_sep_indent(Sep), <<"title=\"">>, E, <<"\"">> ];

render({occi_relations, E}, Sep) when is_list(E) ->
    [ <<";">>, render_sep_indent(Sep), <<"rel=\"">>, render_ssi(E, fun render/2, Sep), <<"\"">> ];

render({occi_location, E}, Sep) ->
    [ <<";">>, render_sep_indent(Sep), <<"location=\"">>, E, <<"\"">> ];

render({occi_attributes, L}, Sep) ->
    [ <<";">>, render_sep_indent(Sep), <<"attributes=\"">>, render_ssi(L, fun render/2, Sep), <<"\"">> ];

render({occi_actions_spec, L}, Sep) ->
    [ <<";">>, render_sep_indent(Sep), <<"actions=\"">>, render_ssi(L, fun render/2, Sep), <<"\"">> ];

render({occi_relation, Scheme, Term}, _Sep) ->
    [ atom_to_list(Scheme), atom_to_list(Term) ];

render({occi_attribute, K, [], _F}, _Sep) ->
    atom_to_list(K);
render({occi_attribute, K, L, _F}, _Sep) ->
    [ atom_to_list(K), <<"{">>, render_attr_properties(L), <<"}">> ];
render({occi_attribute, K, _F}, _Sep) ->
    atom_to_list(K);

render({occi_action_spec, Scheme, Term, _Desc, _Attrs}, _Sep) ->
    [ Scheme, atom_to_list(Term) ];

render({_, undefined}, _Sep) ->
    <<>>;

render(O, _Sep) ->
    lager:error("Invalid value: ~p~n", [O]),
    throw({error, {occi_syntax, "invalid value"}}).

% Render space-separated items
render_ssi(L, F, Sep) ->
    render_ssi(L, F, [], Sep).

render_ssi([], _, Acc, _Sep) ->
    lists:reverse(Acc);
render_ssi([E], F, Acc, Sep) ->
    lists:reverse([F(E, Sep) | Acc]);
render_ssi([H|T], F, Acc, Sep) ->
    render_ssi(T, F, [<<" ">>, F(H, Sep) | Acc], Sep).

render_attr_properties([]) ->
    <<>>;
render_attr_properties([H | T]) when T == [] ->
    atom_to_list(H);
render_attr_properties([ H | T ]) ->
    [ atom_to_list(H), <<",">>, render_attr_properties(T) ].

render_sep('\n') ->
    <<"\n">>;
render_sep(' ') ->
    <<" ">>;
render_sep(_) ->
    <<" ">>.

render_sep_indent('\n') ->
    <<"\n\t">>;
render_sep_indent(' ') ->
    <<" ">>;
render_sep_indent(_) ->
    <<" ">>.
