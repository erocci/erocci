%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 30 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_renderer_json).
-compile({parse_transform, lager_transform}).

-behaviour(occi_renderer).

-include("occi.hrl").

-export([render/1, parse/1]).

%%%
%%% API
%%%
render(Obj) when is_record(Obj, occi_resource); 
		 is_record(Obj, occi_link);
		 is_record(Obj, occi_cid) ->
    Ejson = render_ejson(Obj),
    jiffy:encode(Ejson).

parse(_Bin) ->
    {}.

%%%
%%% Private
%%%
render_ejson(#occi_resource{}=Res) ->
    render_list([{categories, lists:map(fun render_ejson/1, 
					[Res#occi_resource.cid | Res#occi_resource.mixins])
		 }
		 ,{'occi.core.id', Res#occi_resource.id}
		 ,{'occi.core.title', Res#occi_resource.title}
		 ,{'occi.core.summary', Res#occi_resource.summary}
		 ,{attributes, {lists:map(fun({Key, Val}) -> {Key, Val} end, Res#occi_resource.attributes)}}
		]);
render_ejson(#occi_link{}=_Link) ->
    render_list([]);
render_ejson(#occi_cid{}=Cid) ->
    render_list([{scheme, Cid#occi_cid.scheme}, {term, Cid#occi_cid.term}, {class, Cid#occi_cid.class}]).

render_list(L) ->
    {render_list(L, [])}.

render_list([], Acc) ->
    lists:reverse(Acc);
render_list([{_Key, undefined}|Tail], Acc) ->
    render_list(Tail, Acc);
render_list([{_Key, <<>>}|Tail], Acc) ->
    render_list(Tail, Acc);
render_list([{_Key, []}|Tail], Acc) ->
    render_list(Tail, Acc);
render_list([{Key, Val}|Tail], Acc) ->
    render_list(Tail, [{Key, Val}|Acc]).
