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

-export([render_query/1]).
-export([render/1, parse/1]).

%%%
%%% API
%%%
render_query(Categories) ->
    Content = {<<"categories">>, lists:map(fun(Obj) -> 
						   render_ejson(Obj) 
					   end, Categories)},
    jiffy:encode({[Content]}, [pretty]).

render(Obj) when is_record(Obj, occi_category); 
		 is_record(Obj, occi_action); 
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
render_ejson(#occi_category{id=#occi_cid{class=kind}, location=Uri}=Kind) ->
    render_list([{term, occi_kind:get_term(Kind)}
		 ,{scheme, occi_kind:get_scheme(Kind)}
		 ,{class, kind}
		 ,{title, occi_kind:get_title(Kind)}
		 ,{parent, render_cid_uri(occi_kind:get_parent(Kind))}
		 ,{attributes, render_attribute_specs(occi_kind:get_attributes(Kind))}
		 ,{actions, lists:map(fun(Action) -> 
					      render_cid_uri(occi_action:get_id(Action)) 
				      end, occi_kind:get_actions(Kind))}
		 ,{location, Uri}
		]);

render_ejson(#occi_category{id=#occi_cid{class=mixin}, location=Uri}=Mixin) ->
    render_list([{term, occi_mixin:get_term(Mixin)}
		 ,{scheme, occi_mixin:get_scheme(Mixin)}
		 ,{class, mixin}
		 ,{depends, lists:map(fun(Cid) -> render_cid_uri(Cid) end, 
				      occi_mixin:get_depends(Mixin))}
		 ,{applies, lists:map(fun(Cid) -> render_cid_uri(Cid) end, 
				      occi_mixin:get_applies(Mixin))}
		 ,{title, occi_mixin:get_title(Mixin)}
		 ,{attributes, render_attribute_specs(occi_mixin:get_attributes(Mixin))}
		 ,{actions, lists:map(fun(Action) -> 
					      render_cid_uri(occi_action:get_id(Action))
				      end, occi_mixin:get_actions(Mixin))}
		 ,{location, Uri}]);

render_ejson(#occi_action{}=Action) ->
    render_list([{term, occi_action:get_term(Action)}
		 ,{scheme, occi_action:get_scheme(Action)}
		 ,{class, action}
		 ,{title, occi_action:get_title(Action)}
		 ,{attributes, render_attribute_specs(occi_action:get_attributes(Action))}
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
    {[ render_attribute_spec(Key, dict:fetch(Key, Attributes)) 
       || Key <- dict:fetch_keys(Attributes) ]}.

render_attribute_spec(Key, Attr) ->
    L = [
	 {mutable, not occi_attribute:is_immutable(Attr)},
	 {required, occi_attribute:is_required(Attr)},
	 {type, occi_attribute:get_type_id(Attr)},
	 {default, occi_attribute:get_default(Attr)}
	],
    {Key, render_list(L)}.

render_uri(Uri) ->
    occi_types:join_path([<<"">> | Uri]).
