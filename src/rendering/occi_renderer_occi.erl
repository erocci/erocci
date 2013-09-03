%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_renderer_occi).
-compile({parse_transform, lager_transform}).

-behaviour(occi_renderer).

-include("occi.hrl").

%% API
-export([render/1, parse/1]).

%%%===================================================================
%%% API
%%%===================================================================
render(Category) when is_record(Category, occi_kind); 
		      is_record(Category, occi_mixin);
		      is_record(Category, occi_action) ->
    occi_renderer_text:render(Category, " ");
render(Categories) ->
    occi_renderer:join(lists:map(fun(Cat) -> render(Cat) end, Categories), ", ").

parse(_Bin) ->
    {}.
