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

-export([render/1]).

-type ejson()        :: ejson_arr() | ejson_obj().
-type ejson_arr()    :: [ ejson_elem() ].
-type ejson_elem()   :: ejson_obj() | ejson_arr() | ejson_val().
-type ejson_obj()    :: {[ ejson_member() ]}.
-type ejson_member() :: {ejson_key(), ejson_obj() | ejson_arr() | ejson_val()}.
-type ejson_key()    :: binary() | atom().
-type ejson_val()    :: atom() | binary() | string().

%%%
%%% API
%%%
render(#occi_node{type=dir}=Node) ->
    jiffy:encode(lists:flatten(render_dir(Node)), [pretty]);

render(#occi_node{type=occi_resource, data=Res}) ->
    Content = {<<"resources">>, [render_ejson(Res)]},
    jiffy:encode({[Content]}, [pretty]);

render(#occi_node{type=occi_link, data=Link}) ->
    Content = {<<"links">>, [render_ejson(Link)]},
    jiffy:encode({[Content]}, [pretty]);

render(#occi_node{type=occi_query, data={Kinds, Mixins, Actions}}) ->
    KindsJson = {<<"kinds">>, lists:map(fun(Obj) -> 
						render_ejson(Obj) 
					end, Kinds)},
    MixinsJson = {<<"mixins">>, lists:map(fun(Obj) -> 
						  render_ejson(Obj) 
					  end, Mixins)},
    ActionsJson = {<<"actions">>, lists:map(fun(Obj) -> 
						    render_ejson(Obj) 
					    end, Actions)},
    jiffy:encode({[KindsJson, MixinsJson, ActionsJson]}, [pretty]);

render(#occi_node{type=occi_user_mixin, data=Mixin}) ->
    MixinJson = render_ejson(Mixin),
    jiffy:encode(MixinJson, [pretty]);

render(#occi_node{type=occi_collection, data=Coll}) ->
    jiffy:encode([ occi_uri:to_binary(Id) || Id <- occi_collection:get_entities(Coll) ], [pretty]).

%%%
%%% Private
%%%
render_ejson(#occi_kind{location=Uri}=Kind) ->
    strip_list([{term, occi_kind:get_term(Kind)}
		,{scheme, occi_kind:get_scheme(Kind)}
		,{class, kind}
		,{title, occi_kind:get_title(Kind)}
		,{parent, render_cid_uri(occi_kind:get_parent(Kind))}
		,{attributes, render_attribute_specs(occi_kind:get_attr_list(Kind))}
		,{actions, lists:map(fun(Action) -> 
					     render_cid_uri(occi_action:get_id(Action)) 
				     end, occi_kind:get_actions(Kind))}
		,{location, occi_uri:to_binary(Uri)}
	       ]);

render_ejson(#occi_mixin{location=Uri}=Mixin) ->
    strip_list([{term, occi_mixin:get_term(Mixin)}
		,{scheme, occi_mixin:get_scheme(Mixin)}
		,{class, mixin}
		,{depends, lists:map(fun(Cid) -> render_cid_uri(Cid) end, 
				     occi_mixin:get_depends(Mixin))}
		,{applies, lists:map(fun(Cid) -> render_cid_uri(Cid) end, 
				     occi_mixin:get_applies(Mixin))}
		,{title, occi_mixin:get_title(Mixin)}
		,{attributes, render_attribute_specs(occi_mixin:get_attr_list(Mixin))}
		,{actions, lists:map(fun(Action) -> 
					     render_cid_uri(occi_action:get_id(Action))
				     end, occi_mixin:get_actions(Mixin))}
		,{location, occi_uri:to_binary(Uri)}]);

render_ejson(#occi_action{}=Action) ->
    strip_list([{term, occi_action:get_term(Action)}
		,{scheme, occi_action:get_scheme(Action)}
		,{class, action}
		,{title, occi_action:get_title(Action)}
		,{attributes, render_attribute_specs(occi_action:get_attr_list(Action))}
	       ]);

render_ejson(#occi_resource{}=Res) ->
    strip_list([{kind, render_cid_uri(occi_resource:get_cid(Res))}
		,{mixins, sets:fold(fun (Id, Acc) ->
					    [render_cid_uri(Id)|Acc]
				    end, [], occi_resource:get_mixins(Res))}
		,{attributes, render_attribute_values(occi_resource:get_attributes(Res))}
		,{id, occi_uri:to_binary(occi_resource:get_id(Res))}
		,{links, lists:map(fun (#uri{}=Link) ->
					   occi_uri:to_binary(Link);
				       (#occi_link{}=Link) ->
					   render_ejson(Link)
				   end, occi_resource:get_links(Res))}
	       ]);

render_ejson(#occi_link{}=Link) ->
    strip_list([{kind, render_cid_uri(occi_link:get_cid(Link))}
		,{mixins, sets:fold(fun (Id, Acc) ->
					    [render_cid_uri(Id)|Acc]
				    end, [], occi_link:get_mixins(Link))}
		,{attributes, render_attribute_values(occi_link:get_attributes(Link))}
		,{id, occi_uri:to_binary(occi_link:get_id(Link))}
		,{source, occi_uri:to_binary(occi_link:get_source(Link))}
		,{target, occi_uri:to_binary(occi_link:get_target(Link))}		
	       ]);

render_ejson(#occi_cid{}=Cid) ->
    strip_list([{scheme, list_to_binary(atom_to_list(Cid#occi_cid.scheme))}, 
		{term, Cid#occi_cid.term}, 
		{class, Cid#occi_cid.class}]).

render_cid_uri(undefined) ->
    undefined;
render_cid_uri(#occi_cid{}=Cid) ->
    BTerm = list_to_binary(atom_to_list(Cid#occi_cid.term)),
    BScheme = list_to_binary(atom_to_list(Cid#occi_cid.scheme)),
    << BScheme/binary, BTerm/binary >>.

render_attribute_specs(Attrs) ->
    render_attribute_specs(Attrs, {[]}).

render_attribute_specs([], Acc) ->
    Acc;
render_attribute_specs([#occi_attr{}=Attr|Tail], Acc) ->
    L = [
	 {mutable, not occi_attribute:is_immutable(Attr)},
	 {title, occi_attribute:get_title(Attr)},
	 {required, occi_attribute:is_required(Attr)},
	 {type, occi_attribute:get_type_id(Attr)},
	 {default, occi_attribute:get_default(Attr)}
	],
    Id = split_attr_id(occi_attribute:get_id(Attr)),
    render_attribute_specs(Tail, insert_attr(Id, strip_list(L), Acc)).

render_attribute_values(Attr) ->
    render_attribute_values(Attr, {[]}).

render_attribute_values([], Acc) ->
    Acc;
render_attribute_values([#occi_attr{}=Attr|Tail], Acc) ->
    Id = split_attr_id(occi_attribute:get_id(Attr)),
    case occi_attribute:get_value(Attr) of
	undefined ->
	    render_attribute_values(Tail, Acc);
	Value when is_list(Value) ->
	    render_attribute_values(Tail, insert_attr(Id, list_to_binary(Value), Acc));
	Value ->
	    render_attribute_values(Tail, insert_attr(Id, Value, Acc))
    end.

render_dir(#occi_node{type=dir, data=Children}) ->
    gb_sets:fold(fun (#occi_node{type=dir}=Child, Acc) ->
			 [ render_dir(Child) | Acc ];
		     (#occi_node{id=ChildId}, Acc) ->
			 [ occi_uri:to_binary(ChildId) | Acc ]
		 end, [], Children).

%
% insert attribute name/value into ejson tree.
% Name is a list of binary
%
-spec insert_attr(Name :: [binary()], Value :: term(), ejson()) -> ejson().
insert_attr([ Name ], Value, {Tree}) ->
    { [{Name, Value} | Tree ]};
insert_attr([ Name | Tail ], Value, {[ {Name, Children} | OtherNS ]}) ->
    { [{Name, insert_attr(Tail, Value, Children) } | OtherNS] };
insert_attr([ Name | Tail ], Value, {Tree}) ->
    { [{Name, insert_attr(Tail, Value, {[]}) } | Tree] }.

split_attr_id(Name) when is_atom(Name) ->
    lists:map(fun (T) ->
		      list_to_binary(T)
	      end, string:tokens(atom_to_list(Name), ".")).

strip_list(L) ->
    {strip_list(L, [])}.

strip_list([], Acc) ->
    lists:reverse(Acc);
strip_list([{_Key, undefined}|Tail], Acc) ->
    strip_list(Tail, Acc);
strip_list([{_Key, <<>>}|Tail], Acc) ->
    strip_list(Tail, Acc);
strip_list([{_Key, []}|Tail], Acc) ->
    strip_list(Tail, Acc);
strip_list([{_Key, {[]}}|Tail], Acc) ->
    strip_list(Tail, Acc);
strip_list([{Key, Val}|Tail], Acc) ->
    strip_list(Tail, [{Key, Val}|Acc]).
