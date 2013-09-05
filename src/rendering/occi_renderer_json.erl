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
%%% @doc use EEP1108 format ofr JSON internal representation
%%% http://www.erlang.org/eeps/eep-0018.html
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
render(Obj) when is_record(Obj, occi_kind); 
		 is_record(Obj, occi_mixin); 
		 is_record(Obj, occi_action); 
		 is_record(Obj, occi_resource); 
		 is_record(Obj, occi_link);
		 is_record(Obj, occi_cid) ->
    Ejson = render_ejson(Obj),
    jiffy:encode(Ejson);
render(List) ->
    lists:map(fun(Obj) -> render(Obj) end, List).

parse(_Bin) ->
    {}.

%%%
%%% Private
%%%
render_ejson(#occi_kind{}=Kind) ->
    render_list([{category, render_ejson(Kind#occi_kind.id)}
		 ,{title, Kind#occi_kind.title}
		 ,{rel, render_rel(Kind#occi_kind.rel)}
		 ,{attributes, lists:map(fun(X) -> render_attr_spec(X) end, Kind#occi_kind.attributes)}
		 ,{actions, lists:map(fun({S, T, _, _}) -> render_uri([S,T]) end, Kind#occi_kind.actions)}
		 ,{location, render_uri(Kind#occi_kind.location)}
		]);

render_ejson(#occi_mixin{}=Mixin) ->
    render_list([{category, render_ejson(Mixin#occi_mixin.id)}
		 ,{title, Mixin#occi_mixin.title}
		 ,{attributes, lists:map(fun(X) -> render_attr_spec(X) end, Mixin#occi_mixin.attributes)}
		 ,{actions, lists:map(fun({S, T, _, _}) -> render_uri([S,T]) end, Mixin#occi_mixin.actions)}
		 ,{location, render_uri(Mixin#occi_mixin.location)}]);

render_ejson(#occi_action{}=Action) ->
    render_list([{category, render_ejson(Action#occi_action.id)}
		 ,{title, Action#occi_action.title}
		 ,{attributes, lists:map(fun(X) -> render_attr_spec(X) end, Action#occi_action.attributes)}
		]);

render_ejson(#occi_resource{}=Res) ->
    render_list([{categories, lists:map(fun render_ejson/1, 
					[Res#occi_resource.cid | Res#occi_resource.mixins])
		 }
		 ,{'occi.core.id', Res#occi_resource.id}
		 ,{'occi.core.title', Res#occi_resource.title}
		 ,{'occi.core.summary', Res#occi_resource.summary}
		 ,{attributes, {lists:map(fun({Key, Val}) -> {Key, Val} end, Res#occi_resource.attributes)}}
		 ,{location, render_uri(occi_renderer:to_url(Res#occi_resource.id))}
		]);

render_ejson(#occi_link{}=_Link) ->
    render_list([]);

render_ejson(#occi_cid{}=Cid) ->
    render_list([{scheme, render_uri(Cid#occi_cid.scheme)}, {term, Cid#occi_cid.term}, {class, Cid#occi_cid.class}]).

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

render_attr_spec({K, L, _F}) ->
    render_list([
		 {'name', list_to_binary(atom_to_list(K))}
		 ,{'properties', lists:map(fun(X) -> list_to_binary(atom_to_list(X)) end, L)}
		]).

render_rel({Scheme, Term}) ->
    render_uri([Scheme, Term]).

render_uri(Uri) ->
    occi_renderer:to_uri(Uri).
