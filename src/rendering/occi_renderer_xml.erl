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
-include_lib("exmpp/include/exmpp_xml.hrl").

-export([render_capabilities/3,
	 render_collection/1]).

%%%
%%% API
%%%
render_capabilities(Kinds, Mixins, Actions) ->
    Children = lists:map(fun render_kind/1, Kinds)
	++ lists:map(fun render_mixin/1, Mixins)
	++ lists:map(fun render_action/1, Actions),
    render_xml(
      exmpp_xml:set_children(
	exmpp_xml:element(occi, component), Children)).

render_collection(#occi_collection{}=Coll) ->
    render_xml(
      exmpp_xml:set_children(
       exmpp_xml:element(occi, collection),
       lists:map(fun (#occi_resource{}=Res) ->
			 E = exmpp_xml:element(resource),
			 exmpp_xml:set_attribute(E, <<"id">>, occi_resource:get_id(Res))
		 end, occi_collection:get_resources(Coll)))).

%%%
%%% Private
%%%
render_kind(#occi_kind{}=Kind) ->
    E = render_category(exmpp_xml:element(kind), Kind),
    E2 = render_parent(E, occi_kind:get_parent(Kind)),
    render_attr_specs(E2, occi_kind:get_attr_list(Kind)).

render_mixin(#occi_mixin{}=Mixin) ->
    E = render_category(exmpp_xml:element(mixin), Mixin),
    E2 = render_depends(E, occi_mixin:get_depends(Mixin)),
    E3 = render_applies(E2, occi_mixin:get_applies(Mixin)),
    render_attr_specs(E3, occi_mixin:get_attr_list(Mixin)).

render_category(#xmlel{}=E, #occi_kind{location=Uri}=Kind) ->
    render_category(E, 
		    occi_kind:get_scheme(Kind),
		    occi_kind:get_term(Kind),
		    occi_kind:get_title(Kind),
		    Uri);
render_category(#xmlel{}=E, #occi_mixin{location=Uri}=Mixin) ->
    render_category(E, 
		    occi_mixin:get_scheme(Mixin),
		    occi_mixin:get_term(Mixin),
		    occi_mixin:get_title(Mixin),
		    Uri).

render_category(E, Scheme, Term, Title, Uri) ->
    set_attributes(E, [{<<"scheme">>, Scheme}, {<<"term">>, Term}, {<<"title">>, Title}, {<<"location">>, Uri}]).

render_action(#occi_action{location=Uri}=Action) ->
    Attrs = [{<<"scheme">>, occi_action:get_scheme(Action)},
	     {<<"term">>, occi_action:get_term(Action)},	     
	     {<<"title">>, occi_action:get_title(Action)},
	     {<<"location">>, Uri}],
    E = set_attributes(exmpp_xml:element(action), Attrs),
    render_attr_specs(E, occi_action:get_attr_list(Action)).

render_parent(E, undefined) ->
    E;
render_parent(E, #occi_cid{scheme=Scheme, term=Term}) ->
    P = set_attributes(exmpp_xml:element(parent), 
		       [{<<"scheme">>, Scheme},
			{<<"term">>, Term}]),
    exmpp_xml:append_child(E, P).

render_depends(E, []) ->
    E;
render_depends(E, [#occi_cid{scheme=Scheme, term=Term}|Tail]) ->
    Child = set_attributes(exmpp_xml:element(depends), 
		       [{<<"scheme">>, Scheme},
			{<<"term">>, Term}]),
    render_depends(exmpp_xml:append_child(E, Child), Tail).

render_applies(E, []) ->
    E;
render_applies(E, [#occi_cid{scheme=Scheme, term=Term}|Tail]) ->
    Child = set_attributes(exmpp_xml:element(applies), 
			   [{<<"scheme">>, Scheme},
			    {<<"term">>, Term}]),
    render_applies(exmpp_xml:append_child(E, Child), Tail).

render_attr_specs(E, []) ->
    E;
render_attr_specs(E, [#occi_attr{}=A|Tail]) ->
    Attrs = [{<<"name">>, occi_attribute:get_id(A)},
	     {<<"title">>, occi_attribute:get_title(A)},
	     {<<"default">>, occi_attribute:get_default(A)},
	     {<<"use">>, case occi_attribute:is_required(A) of
				     true -> required;
				     false -> undefined
			 end},
	     {<<"immutable">>, case occi_attribute:is_immutable(A) of
				   true -> true;
				   false -> undefined
			       end}],
    Child = set_attributes(exmpp_xml:element(attribute), Attrs),
    Child2 = render_attr_type(Child, A),
    render_attr_specs(exmpp_xml:append_child(E, Child2), Tail).

render_attr_type(#xmlel{}=E, #occi_attr{}=A) ->
    exmpp_xml:set_attribute(E, <<"type">>, occi_attribute:get_type_id(A)).

set_attributes(#xmlel{}=E, []) ->
    E;
set_attributes(#xmlel{}=E, [{_Name, undefined}|Tail]) ->
    set_attributes(E, Tail);
set_attributes(#xmlel{}=E, [{Name, Value}|Tail]) ->
    set_attributes(exmpp_xml:set_attribute(E, Name, Value), Tail).

render_xml(#xmlel{}=Doc) ->
    exmpp_xml:document_to_iolist(
      exmpp_xml:indent_document(Doc, <<"  ">>)).
