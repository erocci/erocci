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
-module(occi_renderer_xml).
-compile({parse_transform, lager_transform}).

-behaviour(occi_renderer).

-include("occi.hrl").
-include("occi_xml.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").

-export([render/1]).

-define(declared_occi_ns, {?occi_ns, none}).
-define(declared_xlink_ns, {?xlink_ns, "xl"}).

%%%
%%% API
%%%
render(#occi_node{type=dir}=Node) ->
    E = make_dir(Node),
    render_xml(
      exmpp_xml:declare_ns_here(
	exmpp_xml:declare_ns_here(E, ?xlink_ns, "xl"),
	?occi_ns, none));

render(#occi_node{type=occi_resource, data=Res}) ->
    E = make_doc([?declared_occi_ns, ?declared_xlink_ns],
		 ?occi_ns, resource,
		 [exmpp_xml:attribute(<<"id">>, occi_uri:to_binary(occi_resource:get_id(Res))),
		  exmpp_xml:attribute(<<"title">>, occi_resource:get_title(Res))], 
		 [make_cid(kind, occi_resource:get_cid(Res))]),
    render_xml(
      lists:foldl(
	fun (#uri{}=Link, Acc) -> 
		render_rel(Acc, link, Link);
	    (#occi_link{}=Link, Acc) ->
		exmpp_xml:append_child(Acc, make_link(Link))
	end, 
	lists:foldl(
	  fun (Attr, Acc) -> render_attribute(Acc, Attr) end, 
	  sets:fold(
	    fun (Mixin, Acc) -> render_cid(Acc, mixin, Mixin) end,
	    E, occi_resource:get_mixins(Res)),
	  occi_resource:get_attributes(Res)),
	occi_resource:get_links(Res)));

render(#occi_node{type=occi_link, data=Link}) ->
    render_xml(make_link(Link));

render(#occi_node{type=occi_query, data={Kinds, Mixins, Actions}}) ->
    Children = lists:map(fun render_kind/1, Kinds)
	++ lists:map(fun render_mixin/1, Mixins)
	++ lists:map(fun render_action/1, Actions),
    render_xml(
      make_doc([?declared_occi_ns],
	       ?occi_ns, component,
	       [],
	       Children));

render(#occi_node{type=occi_user_mixin, data=Mixin}) ->
    render_xml(render_mixin(Mixin));
	  
render(#occi_node{type=occi_collection, data=Coll}) ->
    render_xml(
      make_doc([?declared_occi_ns, ?declared_xlink_ns],
	       ?occi_ns, collection,
	       [],
	       [ exmpp_xml:element(
		   ?occi_ns, entity,
		   [exmpp_xml:attribute(?xlink_ns, <<"href">>, occi_uri:to_binary(Id))], []) || 
		   Id <- occi_collection:get_entities(Coll) ])).

%%%
%%% Private
%%%
make_link(#occi_link{}=Link) ->
    Children = [make_cid(kind, occi_link:get_cid(Link)),
		make_attribute('occi.core.target', occi_link:get_target(Link))],
    Children2 = case occi_link:get_source(Link) of
		    undefined -> Children;
		    #uri{}=Uri ->
			make_attribute('occi.core.source', Uri)
		end,
    E = make_doc([?declared_occi_ns, ?declared_xlink_ns],
		 ?occi_ns, link,
		 [exmpp_xml:attribute(<<"id">>, occi_uri:to_string(occi_link:get_id(Link))),
		  exmpp_xml:attribute(<<"title">>, occi_link:get_title(Link))],
		 Children2),
    sets:fold(
      fun (Mixin, Acc) -> render_cid(Acc, mixin, Mixin) end, 
      lists:foldl(
	fun (Attr, Acc) -> render_attribute(Acc, Attr) end,
	E,
	occi_link:get_attributes(Link)), 
      occi_link:get_mixins(Link)).

make_dir(#occi_node{id=Id, type=dir, data=Children}) ->
    exmpp_xml:element(?occi_ns, collection, 
		      [exmpp_xml:attribute(<<"id">>, occi_uri:to_binary(Id))],
		      gb_sets:fold(fun (Child, Acc) ->
					   [ make_dir(Child) | Acc ]
				   end, [], Children));
make_dir(#uri{}=Id) ->
    exmpp_xml:element(?occi_ns, entity,
		      [exmpp_xml:attribute(?xlink_ns, <<"href">>, occi_uri:to_binary(Id))], 
		      []).

render_kind(#occi_kind{}=Kind) ->
    E = render_category(exmpp_xml:element(?occi_ns, kind), Kind),
    E2 = render_parent(E, occi_kind:get_parent(Kind)),
    render_attr_specs(E2, occi_kind:get_attr_list(Kind)).

render_mixin(#occi_mixin{}=Mixin) ->
    E = render_category(exmpp_xml:element(?occi_ns, mixin), Mixin),
    E2 = render_depends(E, occi_mixin:get_depends(Mixin)),
    E3 = render_applies(E2, occi_mixin:get_applies(Mixin)),
    render_attr_specs(E3, occi_mixin:get_attr_list(Mixin)).

render_category(#xmlel{}=E, #occi_kind{location=#uri{}=Uri}=Kind) ->
    render_category(E, 
		    occi_kind:get_scheme(Kind),
		    occi_kind:get_term(Kind),
		    occi_kind:get_title(Kind),
		    occi_uri:to_binary(Uri));
render_category(#xmlel{}=E, #occi_mixin{location=#uri{}=Uri}=Mixin) ->
    render_category(E, 
		    occi_mixin:get_scheme(Mixin),
		    occi_mixin:get_term(Mixin),
		    occi_mixin:get_title(Mixin),
		    occi_uri:to_binary(Uri)).

render_category(E, Scheme, Term, Title, Uri) ->
    set_attributes(E, [{<<"scheme">>, Scheme}, {<<"term">>, Term}, 
		       {<<"title">>, Title}, {<<"location">>, Uri}]).

render_action(#occi_action{location=Uri}=Action) ->
    Attrs = [{<<"scheme">>, occi_action:get_scheme(Action)},
	     {<<"term">>, occi_action:get_term(Action)},	     
	     {<<"title">>, occi_action:get_title(Action)},
	     {<<"location">>, occi_uri:to_binary(Uri)}],
    E = set_attributes(exmpp_xml:element(?occi_ns, action), Attrs),
    render_attr_specs(E, occi_action:get_attr_list(Action)).

render_cid(E, _, undefined) ->
    E;
render_cid(E, Name, #occi_cid{}=Cid) ->
    exmpp_xml:append_child(E, make_cid(Name, Cid)).

make_cid(Name, #occi_cid{scheme=Scheme, term=Term}) ->
    exmpp_xml:element(?occi_ns, Name, 
		      [exmpp_xml:attribute(<<"scheme">>, Scheme),
		       exmpp_xml:attribute(<<"term">>, Term)],
		      []).

render_rel(E, _, undefined) ->
    E;
render_rel(E, Name, #uri{}=Uri) ->
    exmpp_xml:append_child(E, make_rel(Name, Uri)).

make_rel(Name, #uri{}=Uri) ->
    exmpp_xml:element(?occi_ns, Name, 
		      [exmpp_xml:attribute(?xlink_ns, <<"href">>, occi_uri:to_binary(Uri))],
		      []).

render_parent(E, Cid) ->
    render_cid(E, parent, Cid).

render_depends(E, []) ->
    E;
render_depends(E, [#occi_cid{}=Cid|Tail]) ->
    render_depends(
      render_cid(E, depends, Cid), Tail).

render_applies(E, []) ->
    E;
render_applies(E, [#occi_cid{}=Cid|Tail]) ->
    render_applies(
      render_cid(E, applies, Cid), Tail).

render_attr_specs(E, []) ->
    E;
render_attr_specs(E, [#occi_attr{}=A|Tail]) ->
    render_attr_specs(
      exmpp_xml:append_child(E, make_attr_spec(A)), Tail).

make_attr_spec(#occi_attr{}=A) ->
    Attrs = [exmpp_xml:attribute(<<"name">>, occi_attribute:get_id(A)),
	     exmpp_xml:attribute(<<"title">>, occi_attribute:get_title(A)),
	     exmpp_xml:attribute(<<"default">>, occi_attribute:get_default(A)),
	     exmpp_xml:attribute(<<"use">>, case occi_attribute:is_required(A) of
						true -> required;
						false -> undefined
					    end),
	     exmpp_xml:attribute(<<"immutable">>, case occi_attribute:is_immutable(A) of
						      true -> true;
						      false -> undefined
						  end),
	     exmpp_xml:attribute(<<"type">>, occi_attribute:get_type_id(A))],
    exmpp_xml:element(?occi_ns, attribute, Attrs, []).

render_attribute(E, #occi_attr{}=Attr) ->
    exmpp_xml:append_child(E, make_attribute(Attr)).

make_attribute(#occi_attr{}=Attr) ->
    make_attribute(occi_attribute:get_id(Attr), occi_attribute:get_value(Attr)).

make_attribute(Name, #uri{}=Uri) ->
    exmpp_xml:element(?occi_ns, attribute, 
		      [exmpp_xml:attribute(<<"name">>, Name),
		       exmpp_xml:attribute(?xlink_ns,
			 <<"href">>, occi_uri:to_binary(Uri))],
		      []);
make_attribute(Name, Value) ->
    exmpp_xml:element(?occi_ns, attribute, 
		      [exmpp_xml:attribute(<<"name">>, Name),
		       exmpp_xml:attribute(<<"value">>, render_attr_value(Value))],
		      []).

set_attributes(#xmlel{}=E, []) ->
    E;
set_attributes(#xmlel{}=E, [{_Name, undefined}|Tail]) ->
    set_attributes(E, Tail);
set_attributes(#xmlel{}=E, [{Name, Value}|Tail]) ->
    set_attributes(exmpp_xml:set_attribute(E, Name, Value), Tail).

render_xml(#xmlel{}=Doc) ->
    exmpp_xml:document_to_iolist(
      exmpp_xml:indent_document(Doc, <<"  ">>)).

render_attr_value(Val) when is_float(Val) ->
    io_lib:format("~p", [Val]);
render_attr_value(Val) when is_integer(Val) ->
    io_lib:format("~p", [Val]);
render_attr_value(#uri{}=Val) ->
    occi_uri:to_string(Val);
render_attr_value(Val) ->
    Val.

make_doc(DeclaredNS, NS, Name, Attrs, Children) ->
    make_ns(DeclaredNS, exmpp_xml:element(NS, Name, Attrs, Children)).

make_ns([], #xmlel{}=E) ->
    E;
make_ns([{NS, Prefix}|Tail], #xmlel{}=E) ->
    make_ns(Tail, exmpp_xml:declare_ns_here(E, NS, Prefix)).
