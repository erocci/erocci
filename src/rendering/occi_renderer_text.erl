%%%-------------------------------------------------------------------
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
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_renderer_text).
-compile({parse_transform, lager_transform}).

-include("occi.hrl").

%% API
-export([render/3]).

%%%===================================================================
%%% API
%%%===================================================================
render(#occi_node{type=dir}=Node, Req, Renderer) ->
    Headers = render_dir(Node, orddict:from_list([{<<"x-occi-location">>, []}])),
    Renderer(Headers, Req);

render(#occi_node{type=occi_resource, data=Res}, Req, Renderer) ->
    Headers = render_resource(Res, orddict:new()),
    Renderer(Headers, Req);

render(#occi_node{type=occi_link, data=Link}, Req, Renderer) ->
    Headers = render_link(Link, orddict:new()),
    Renderer(Headers, Req);

render(#occi_node{type=occi_query, data={Kinds, Mixins, Actions}}, Req, Renderer) ->
    Headers = lists:foldl(fun (Cat, Acc) ->
				  render_category(Cat, Acc)
			  end, orddict:from_list([{<<"category">>, []}]), Kinds ++ Mixins ++ Actions),
    Renderer(Headers, Req);

render(#occi_node{type=occi_collection, data=Coll}, Req, Renderer) ->
    Headers = lists:foldl(fun (EntityId, Acc) ->
				  Uris = orddict:fetch(<<"x-occi-location">>, Acc),
				  orddict:store(<<"x-occi-location">>, [occi_uri:to_iolist(EntityId) | Uris], Acc)
			  end, 
			  orddict:from_list([{<<"x-occi-location">>, []}]), 
			  occi_collection:get_entities(Coll)),
    Renderer(Headers, Req).

render_category(#occi_kind{}=Kind, Hdr) ->
    L = [build_cid(occi_kind:get_id(Kind)),
	 render_kv(<<"title">>, occi_kind:get_title(Kind)),
	 render_kv(<<"rel">>, render_cid_uri(occi_kind:get_parent(Kind))),
	 render_kv(<<"attributes">>, render_attr_specs(occi_kind:get_attributes(Kind))),
	 render_kv(<<"actions">>, render_action_specs(occi_kind:get_actions(Kind))),
	 render_kv(<<"location">>, [occi_uri:to_iolist(occi_kind:get_location(Kind))])],
    add_header_value(<<"category">>, occi_renderer:join(L, "; "), Hdr);

render_category(#occi_mixin{}=Mixin, Hdr) ->
    L = [build_cid(occi_mixin:get_id(Mixin)),
	 render_kv(<<"title">>, occi_mixin:get_title(Mixin)),
	 render_kv(<<"attributes">>, render_attr_specs(occi_mixin:get_attributes(Mixin))),
	 render_kv(<<"actions">>, render_action_specs(occi_mixin:get_actions(Mixin))),
	 render_kv(<<"location">>, [occi_uri:to_iolist(occi_mixin:get_location(Mixin))])],
    add_header_value(<<"category">>, occi_renderer:join(L, "; "), Hdr);

render_category(#occi_action{}=Action, Hdr) ->
    L = [build_cid(occi_action:get_id(Action)),
	 render_kv(<<"title">>, occi_action:get_title(Action)),
	 render_kv(<<"attributes">>, render_attr_specs(occi_action:get_attributes(Action)))],
    add_header_value(<<"category">>, occi_renderer:join(L, "; "), Hdr).

render_cid(#occi_cid{}=Cid, Acc) ->
    add_header_value(<<"category">>, build_cid(Cid), Acc).

render_dir(#occi_node{type=dir, data=Children}, Acc) ->
    gb_sets:fold(fun (#occi_node{type=dir}=Child, Acc2) ->
			 render_dir(Child, Acc2);
		     (#uri{}=ChildId, Acc2) ->
			 add_header_value(<<"x-occi-location">>, occi_uri:to_iolist(ChildId), Acc2)
		 end, Acc, Children).

render_resource(#occi_resource{}=Res, Acc) ->
    Acc2 = render_cid(occi_resource:get_cid(Res), Acc),
    Acc3 = sets:fold(fun render_cid/2, Acc2, occi_resource:get_mixins(Res)),
    Acc4 = lists:foldl(fun render_attribute/2, Acc3, occi_resource:get_attributes(Res)),
    Acc5 = lists:foldl(fun render_inline_link/2, Acc4, occi_resource:get_links(Res)),
    render_location(occi_resource:get_id(Res), Acc5).

render_link(#occi_link{}=Link, Acc) ->
    Acc2 = render_cid(occi_link:get_cid(Link), Acc),
    Acc3 = sets:fold(fun render_cid/2, Acc2, occi_link:get_mixins(Link)),
    Acc4 = lists:foldl(fun render_attribute/2, Acc3, occi_link:get_attributes(Link)),
    render_location(occi_link:get_id(Link), Acc4).

render_inline_link(#uri{}=Uri, Acc) ->
    add_header_value(<<"link">>, occi_uri:to_iolist(Uri), Acc);
render_inline_link(#occi_link{}=Link, Acc) ->
    add_header_value(<<"link">>, build_inline_link(Link), Acc).

render_location(#uri{}=Uri, Acc) ->
    add_header_value(<<"location">>, occi_uri:to_iolist(Uri), Acc).

render_cid_uri(undefined) ->
    undefined;
render_cid_uri(#occi_cid{scheme=Scheme, term=Term}) ->
    [ atom_to_list(Scheme), atom_to_list(Term) ].

render_attr_specs(Attributes) ->
    occi_renderer:join(orddict:fold(fun render_attr_spec/3, [], Attributes), " ").

render_attr_spec(Id, #occi_attr{}=Attr, Acc) ->
    [ [ atom_to_list(Id), render_attr_properties(Attr) ] | Acc ].

render_attr_properties(#occi_attr{}=A) ->
    L = case occi_attribute:is_required(A) of
	    true -> ["required"];		
	    false -> []
	end,
    L1 = lists:append(L, case occi_attribute:is_immutable(A) of
			     true -> ["immutable"];
			     false -> []
			 end),
    case L1 of
	[] -> [];
	L2 ->
	    [ <<"{">>, occi_renderer:join(L2, <<",">>), <<"}">>]
    end.

render_action_specs(Actions) ->
    occi_renderer:join(
      lists:map(
	fun(Action) -> render_action_spec(Action)  end,
	Actions), " ").

render_action_spec(#occi_action{id=Id}) ->
    render_cid_uri(Id).

render_attribute(#occi_attr{id='occi.core.id'}, Acc) ->
    Acc;
render_attribute(#occi_attr{}=Attr, Acc) ->
    add_header_value(<<"x-occi-attribute">>, build_attribute(Attr), Acc).

render_kv(_Key, undefined) ->
    [];
render_kv(_Key, <<>>) ->
    [];
render_kv(_Key, []) ->
    [];
render_kv(Key, Value) ->
    [Key, "=\"", format_value(Value), "\""].

format_value(V) when is_atom(V) ->
    atom_to_list(V);
format_value(V) when is_integer(V) ->
    integer_to_list(V);
format_value(V) when is_float(V) ->
    io_lib:format("~g", [V]);
format_value(#uri{}=U) ->
    occi_uri:to_iolist(U);
format_value(V) ->
    V.

build_inline_link(#occi_link{}=Link) ->
    L = [ [ "<", occi_uri:to_iolist(occi_link:get_target(Link)), ">" ],
			 render_kv("self", occi_uri:to_iolist(occi_link:get_id(Link))),
			 render_kv("category", render_cid_uri(occi_link:get_cid(Link)))],
    L2 = lists:foldl(fun (Attr, Acc) ->
			     [ build_attribute(Attr) | Acc ]
		     end, L, occi_link:get_attributes(Link)),
    occi_renderer:join(L2, "; ").

build_attribute(#occi_attr{}=Attr) ->
    Name = atom_to_list(occi_attribute:get_id(Attr)),
    render_kv(Name, occi_attribute:get_value(Attr)).

build_cid(#occi_cid{term=Term, scheme=Scheme, class=Cls}) ->
    occi_renderer:join(
      [render_kv(<<"term">>, Term),
       render_kv(<<"scheme">>, Scheme),
       render_kv(<<"class">>, Cls)], "; ").

add_header_value(Name, Value, Acc) ->
    Values = case orddict:find(Name, Acc) of
		 {ok, V} -> V;
		 error -> []
	     end,
    orddict:store(Name, [Value | Values], Acc).
