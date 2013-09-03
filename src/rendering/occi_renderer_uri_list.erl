%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_renderer_uri_list).
-compile({parse_transform, lager_transform}).

-behaviour(occi_renderer).

-include("occi.hrl").

%% API
-export([render/1, parse/1]).

%%%===================================================================
%%% API
%%%===================================================================
render(#occi_kind{location=Location}) ->
    render_uri(Location);
render(#occi_mixin{location=Location}) ->
    render_uri(Location);
render(#occi_action{}) ->
    [];
render(List) ->
    lists:map(fun(Obj) -> render(Obj) end, List).

parse(_Bin) ->
    {}.

%%%
%%% Private
%%%
render_uri(undefined) ->
    [];
render_uri(<<>>) ->
    [];
render_uri([]) ->
    [];
render_uri(Uri) ->
    [ Uri, <<"\n">> ].
