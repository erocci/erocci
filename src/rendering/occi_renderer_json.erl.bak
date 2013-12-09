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
		 is_record(Obj, occi_action_spec); 
		 is_record(Obj, occi_resource); 
		 is_record(Obj, occi_link);
		 is_record(Obj, occi_cid) ->
    Ejson = render_ejson(Obj),
    jiffy:encode(Ejson);
render(List) when is_list(List) ->
    jiffy:encode(lists:map(fun(Obj) -> 
				   render_ejson(Obj) 
			   end, List)).

parse(Bin) ->
    jiffy:decode(Bin).

%%%
%%% Private
%%%
render_ejson(#occi_kind{}=Kind) ->
    render_list([{term, Kind#occi_kind.id#occi_cid.term}
		 ,{scheme, Kind#occi_kind.id#occi_cid.scheme}
		 ,{title, Kind#occi_kind.title}
		 ,{parent, render_cid_uri(Kind#occi_kind.rel)}
		 ,{attributes, render_attribute_specs(Kind#occi_kind.attributes)}
		 ,{actions, lists:map(fun(Action) -> 
					      render_cid_uri(Action#occi_action_spec.id) 
				      end, Kind#occi_kind.actions)}
		 ,{location, render_uri(Kind#occi_kind.location)}
		]);

render_ejson(#occi_mixin{}=Mixin) ->
    render_list([{term, Mixin#occi_mixin.id#occi_cid.term}
		 ,{scheme, Mixin#occi_mixin.id#occi_cid.scheme}
		 ,{depends, lists:map(fun(Cid) -> render_cid_uri(Cid) end, 
				      Mixin#occi_mixin.depends)}
		 ,{applies, lists:map(fun(Cid) -> render_cid_uri(Cid) end, 
				      Mixin#occi_mixin.applies)}
		 ,{title, Mixin#occi_mixin.title}
		 ,{attributes, render_attribute_specs(Mixin#occi_mixin.attributes)}
		 ,{actions, lists:map(fun(Action) -> 
					      render_cid_uri(Action#occi_action_spec.id)
				      end, Mixin#occi_mixin.actions)}
		 ,{location, render_uri(Mixin#occi_mixin.location)}]);

render_ejson(#occi_action_spec{}=Action) ->
    render_list([{term, Action#occi_action_spec.id#occi_cid.term}
		 ,{scheme, Action#occi_action_spec.id#occi_cid.scheme}
		 ,{title, Action#occi_action_spec.title}
		 ,{attributes, render_attribute_specs(Action#occi_action_spec.attributes)}
		]);

render_ejson(#occi_resource{}=Res) ->
    render_list([{kind, render_cid_uri(Res#occi_resource.cid)}
		 ,{categories, lists:map(fun render_ejson/1, 
					[Res#occi_resource.cid | Res#occi_resource.mixins])
		 }
		 ,{'occi.core.id', Res#occi_resource.id}
		 ,{'occi.core.title', Res#occi_resource.title}
		 ,{'occi.core.summary', Res#occi_resource.summary}
		 ,{attributes, {lists:map(fun({Key, Val}) -> {Key, Val} end, Res#occi_resource.attributes)}}
		 ,{location, render_uri(Res#occi_resource.id)}
		]);

render_ejson(#occi_link{}=_Link) ->
    render_list([]);

render_ejson(#occi_cid{}=Cid) ->
    render_list([{scheme, list_to_binary(atom_to_list(Cid#occi_cid.scheme))}, 
		 {term, Cid#occi_cid.term}, 
		 {class, Cid#occi_cid.class}]).

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

render_cid_uri(#occi_cid{}=Cid) ->
    BTerm = list_to_binary(atom_to_list(Cid#occi_cid.term)),
    BScheme = list_to_binary(atom_to_list(Cid#occi_cid.scheme)),
    << BScheme/binary, BTerm/binary >>.

render_attribute_specs(Attributes) ->
    {[ render_attribute_spec(Attr) || Attr <- Attributes ]}.

render_attribute_spec(#occi_attr{}=Spec) ->
    L = [
	 {mutable, not occi_attribute:is_immutable(Spec)},
	 {required, occi_attribute:is_required(Spec)},
	 {type, occi_attribute:get_type(Spec)},
	 {default, occi_attribute:get_default(Spec)},
	 {description, occi_attribute:get_title(Spec)}
	],
    {Spec#occi_attr.id, render_list(L)}.

render_uri(Uri) ->
    occi_types:join_path([<<"">> | Uri]).
