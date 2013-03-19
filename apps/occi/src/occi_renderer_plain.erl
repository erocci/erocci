%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_renderer_plain).

-include("occi.hrl").

-define(ENCODING, latin1).

-export([render/1]).
-export([render_uri/1, 
				 render_attrs/1, render_attr/1,
				 render_actions/1, render_action/1]).

render(#occi_kind{super=Cat, actions=A, rel=R}) ->
		[ 
			render({term, Cat#occi_category.term}),
			render({scheme, Cat#occi_category.scheme}),
			<<";\n\tclass=\"kind\"">>,
			render({title, Cat#occi_category.title}),
			render({rel, R}),
			render({location, Cat#occi_category.location}),
			render({attrs, Cat#occi_category.attrs}),
			render({actions, A}),
			<<"\n">>
		];
render({term, undefined}) ->
		throw({occi, "undefined term"});
render({term, E}) ->
		[ <<"Category: \"">>, atom_to_list(E), <<"\"">> ];
render({scheme, undefined}) ->
		throw({occi, "undefined scheme"});
render({scheme, E}) ->
		[ <<";\n\tscheme=\"">>, E, <<"\"">> ];
render({title, E}) ->
		[ <<";\n\ttitle=\"">>, E, <<"\"">> ];
render({rel, E}) ->
		[ <<";\n\trel=\"">>, E, <<"\"">> ];
render({location, E}) ->
		[ <<";\n\tlocation=\"">>, E, <<"\"">> ];
render({attrs, undefined}) ->
		<<>>;
render({attrs, L}) ->
		[ <<";\n\tattributes=\"">>, render_attrs(L), <<"\"">> ];
render({actions, undefined}) ->
		<<>>;
render({actions, L}) ->
		[ <<";\n\tactions=\"">>, render_actions(L), <<"\"">> ];
render({_, undefined}) ->
		<<>>;
render(_) ->
		throw({occi, "invalid value"}).

%%--------------------------------------------------------------------
%% @doc Check URI has leading and ending '/'
%% @spec U: binary list
%% @end
%%--------------------------------------------------------------------
render_uri(U) ->
		render_uri(U, []).

% Check for leading and ending '/'
render_uri(<<"/", Tail/binary>>, []) ->
		render_uri(Tail, [<<"/">>]);
render_uri(<<_:8, _/binary>>, []) ->
		throw({occi, "invalid uri"});
render_uri(<<"/">>, Acc) ->
		lists:reverse([ <<"/">> | Acc ]);
render_uri(<<H:8, Tail/binary>>, Acc) ->
		render_uri(Tail, [ <<H>> | Acc ]);
render_uri(<<>>, _) ->
		throw({occi, "invalid uri"}).

render_attrs(L) ->
		render_attrs(L, []).

render_attrs([], Acc) ->
		lists:reverse(Acc);
render_attrs([ A ], Acc) ->
		lists:reverse([ render_attr(A) | Acc ]);
render_attrs([ A | T], Acc) ->
		render_attrs(T, [ <<" ">>, render_attr(A) | Acc ]).

render_attr( #occi_attr{ key=K, prop=undefined } ) ->
		atom_to_binary(K, ?ENCODING);
render_attr( #occi_attr{ key=K, prop=immutable } ) ->
		[ atom_to_binary(K, ?ENCODING), <<"{immutable}">> ];
render_attr( #occi_attr{ key=K, prop=required } ) ->
		[ atom_to_binary(K, ?ENCODING), <<"{required}">> ].

render_actions(L) ->
		render_actions(L, []).

render_actions([], Acc) ->
		lists:reverse(Acc);
render_actions([ A ], Acc) ->
		lists:reverse([ render_action(A) | Acc ]);
render_actions([ A | T], Acc) ->
		render_actions(T, [ <<" ">>, render_action(A) | Acc ]).

render_action( #occi_action{ super=Cat } ) ->
		[ Cat#occi_category.scheme, 
			atom_to_binary(Cat#occi_category.term, ?ENCODING) ];
render_action(_) ->
		throw({occi, "invalid action"}).
