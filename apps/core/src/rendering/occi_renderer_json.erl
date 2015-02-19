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

-behaviour(occi_renderer).

-include("occi.hrl").

-export([render/2]).

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
render(#occi_node{id=Id, type=occi_resource, data=Res}, Env) ->
    Content = {<<"resources">>, [render_ejson(Id, Res, Env)]},
    {jiffy:encode({[Content]}, [pretty]), Env};

render(#occi_node{id=Id, type=occi_link, data=Link}, Env) ->
    Content = {<<"links">>, [render_ejson(Id, Link, Env)]},
    {jiffy:encode({[Content]}, [pretty]), Env};

render(#occi_node{id=Id, type=capabilities, data={Kinds, Mixins, Actions}}, Env) ->
    KindsJson = {<<"kinds">>, lists:map(fun(Obj) -> 
						render_ejson(Id, Obj, Env) 
					end, Kinds)},
    MixinsJson = {<<"mixins">>, lists:map(fun(Obj) -> 
						  render_ejson(Id, Obj, Env) 
					  end, Mixins)},
    ActionsJson = {<<"actions">>, lists:map(fun(Obj) -> 
						    render_ejson(Id, Obj, Env) 
					    end, Actions)},
    {jiffy:encode({[KindsJson, MixinsJson, ActionsJson]}, [pretty]), Env};

render(#occi_node{id=Id, type=capabilities, data=Mixin}, Env) ->
    MixinJson = render_ejson(Id, Mixin, Env),
    {jiffy:encode(MixinJson, [pretty]), Env};

render(#occi_node{type=occi_collection, data=#occi_collection{entities=Entities}}, Env) ->
    Content = {<<"collection">>, render_entities(Entities, Env)},
    {jiffy:encode({[Content]}, [pretty]), Env}.


%%%
%%% Private
%%%
render_ejson(_, #occi_kind{location=Uri}=Kind, Env) ->
    { strip_list([{term, occi_kind:get_term(Kind)}
		  ,{scheme, occi_kind:get_scheme(Kind)}
		  ,{class, kind}
		  ,{title, occi_kind:get_title(Kind)}
		  ,{parent, render_cid_uri(occi_kind:get_parent(Kind))}
		  ,{attributes, render_attribute_specs(occi_kind:get_attr_list(Kind), Env)}
		  ,{actions, lists:map(fun(Action) -> 
					       render_cid_uri(occi_action:get_id(Action)) 
				       end, occi_kind:get_actions(Kind))}
		  ,{location, occi_uri:to_binary(Uri, Env)}
		 ]) };

render_ejson(_, #occi_mixin{location=Uri}=Mixin, Env) ->
    { strip_list([{term, occi_mixin:get_term(Mixin)}
		  ,{scheme, occi_mixin:get_scheme(Mixin)}
		  ,{class, mixin}
		  ,{depends, lists:map(fun(Cid) -> render_cid_uri(Cid) end, 
				       occi_mixin:get_depends(Mixin))}
		  ,{applies, lists:map(fun(Cid) -> render_cid_uri(Cid) end, 
				       occi_mixin:get_applies(Mixin))}
		  ,{title, occi_mixin:get_title(Mixin)}
		  ,{attributes, render_attribute_specs(occi_mixin:get_attr_list(Mixin), Env)}
		  ,{actions, lists:map(fun(Action) -> 
					       render_cid_uri(occi_action:get_id(Action))
				       end, occi_mixin:get_actions(Mixin))}
		  ,{location, occi_uri:to_binary(Uri, Env)}]) };

render_ejson(_, #occi_action{}=Action, Env) ->
    { strip_list([{term, occi_action:get_term(Action)}
		  ,{scheme, occi_action:get_scheme(Action)}
		  ,{class, action}
		  ,{title, occi_action:get_title(Action)}
		  ,{attributes, render_attribute_specs(occi_action:get_attr_list(Action), Env)}
		 ]) };

render_ejson(ResId, #occi_resource{}=Res, Env) ->
    { strip_list([{kind, render_cid_uri(occi_resource:get_cid(Res))}
		  ,{id, occi_uri:to_binary(ResId)}
		  ,{mixins, sets:fold(fun (Id, Acc) ->
					      [render_cid_uri(Id)|Acc]
				      end, [], occi_resource:get_mixins(Res))}
		  ,{attributes, render_attribute_values(occi_resource:get_attributes(Res), Env)}
		  ,{links, lists:map(fun (#uri{}=Link) ->
					     occi_uri:to_binary(Link, Env);
					 (#occi_link{id=LinkId}=Link) ->
					     render_ejson(LinkId, Link, Env)
				     end, occi_resource:get_links(Res)) }
		 ]) };

render_ejson(LinkId, #occi_link{}=Link, Env) ->
    Attrs = [ occi_link:get_attr(Link, 'occi.core.source'), 
	      occi_link:get_attr(Link, 'occi.core.target') 
	      | occi_link:get_attributes(Link)],
    { strip_list([{kind, render_cid_uri(occi_link:get_cid(Link))}
		  ,{id, occi_uri:to_binary(LinkId, Env)}
		  ,{mixins, sets:fold(fun (Id, Acc) ->
					      [render_cid_uri(Id)|Acc]
				      end, [], occi_link:get_mixins(Link))}
		  ,{attributes, render_attribute_values(Attrs, Env)}
		 ]) };

render_ejson(_, #occi_cid{}=Cid, _) ->
    { strip_list([{scheme, to_binary(Cid#occi_cid.scheme)}, 
		  {term, Cid#occi_cid.term}, 
		  {class, Cid#occi_cid.class}]) }.


render_cid_uri(undefined) ->
    undefined;
render_cid_uri(#occi_cid{}=Cid) ->
    BTerm = to_binary(Cid#occi_cid.term),
    BScheme = to_binary(Cid#occi_cid.scheme),
    << BScheme/binary, BTerm/binary >>.

render_attribute_specs(Attrs, Env) ->
    render_attribute_specs(Attrs, {[]}, Env).

render_attribute_specs([], Acc, _) ->
    Acc;
render_attribute_specs([#occi_attr{}=Attr|Tail], Acc, Env) ->
    L = [
	 {mutable, not occi_attribute:is_immutable(Attr)},
	 {title, occi_attribute:get_title(Attr)},
	 {required, occi_attribute:is_required(Attr)},
	 {type, occi_attribute:get_type_id(Attr)},
	 {default, occi_attribute:get_default(Attr)}
	],
    Id = split_attr_id(occi_attribute:get_id(Attr)),
    render_attribute_specs(Tail, insert_attr(Id, { strip_list(L) }, Acc), Env).

render_attribute_values(Attr, Env) ->
    render_attribute_values(Attr, {[]}, Env).

render_attribute_values([], Acc, _) ->
    Acc;
render_attribute_values([#occi_attr{}=Attr|Tail], Acc, Env) ->
    Id = split_attr_id(occi_attribute:get_id(Attr)),
    case occi_attribute:get_value(Attr) of
	undefined ->
	    render_attribute_values(Tail, Acc, Env);
	#uri{}=U ->
	    render_attribute_values(Tail, insert_attr(Id, occi_uri:to_binary(U, Env), Acc), Env);
	Value when is_list(Value) ->
	    render_attribute_values(Tail, insert_attr(Id, list_to_binary(Value), Acc), Env);
	Value ->
	    render_attribute_values(Tail, insert_attr(Id, Value, Acc), Env)
    end.


render_entities(Entities, Env) ->
    F = fun (#uri{}=Id, Acc) ->
		[ occi_uri:to_binary(Id, Env) | Acc ];
	    (#occi_node{id=Id, type=occi_resource, data=Res}, Acc) ->
		[ render_ejson(Id, Res, Env) | Acc ];
	    (#occi_node{id=Id, type=occi_link, data=Link}, Acc) ->
		[ render_ejson(Id, Link, Env) | Acc ]
	end,
    ordsets:fold(F, [], Entities).

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

split_attr_id(Name) ->
    [ T || T <- binary:split(to_binary(Name), <<".">>, [global])].

strip_list(L) ->
    strip_list(L, []).

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

to_binary(V) when is_atom(V) -> atom_to_binary(V, latin1);
to_binary(V) when is_binary(V) -> V.
