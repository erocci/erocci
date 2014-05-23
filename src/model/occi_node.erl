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
%%% Created : 24 Jan 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_node).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-export([new/2,
	 new_dir/2,
	 get_objid/1,
	 get_type/1,
	 set_type/2,
	 add_child/2,
	 set_children/2,
	 del_children/2,
	 has_children/1,
	 get_parent/1,
	 set_parent/1,
	 get_data/1,
	 set_data/2,
	 add_prefix/2,
	 rm_prefix/2]).

%%%
%%% API
%%%
-spec new(occi_node_id(), occi_node_type() | occi_object()) -> occi_node().
new(#uri{path=Path}, dir)  ->
    #occi_node{id=#uri{path=Path}, type=dir, data=gb_sets:new()};

new(#uri{path=Path}, #occi_backend{ref=Ref}=Backend)  ->
    NormPath = "/" ++ string:join(string:tokens(Path, "/"), "/"),
    #occi_node{id=#uri{path=NormPath}, objid=Ref, type=mountpoint, data=Backend};

new(#uri{path=Path}, #occi_resource{id=Id}=Data)  ->
    #occi_node{id=#uri{path=Path}, objid=Id, type=occi_resource, data=Data};

new(#uri{path=Path}, #occi_link{id=Id}=Data)  ->
    #occi_node{id=#uri{path=Path}, objid=Id, type=occi_link, data=Data};

new(#uri{path=Path}, #occi_mixin{id=Id}=Data)  ->
    #occi_node{id=#uri{path=Path}, objid=Id, type=occi_user_mixin, data=Data};

new(#uri{path=Path}, #occi_cid{}=Id) ->
    #occi_node{id=#uri{path=Path}, objid=Id, type=occi_collection, data=undefined};

new(#uri{path=Path}, #occi_collection{cid=Id}=Coll) ->
    #occi_node{id=#uri{path=Path}, objid=Id, type=occi_collection, data=Coll};

new(#uri{path=Path}, Type) when is_atom(Type) ->
    #occi_node{id=#uri{path=Path}, type=Type}.

-spec new_dir(uri(), [uri() | occi_node()]) -> occi_node().
new_dir(#uri{path=Path}, Children) when is_list(Children) ->
    #occi_node{id=#uri{path=Path}, type=dir, data=gb_sets:from_list(Children)}.

-spec get_objid(occi_node()) -> any().
get_objid(#occi_node{objid=Id}) ->
    Id.

-spec get_type(occi_node()) -> occi_node_type().
get_type(#occi_node{type=Type}) ->
    Type.

-spec set_type(occi_node(), occi_node_type()) -> occi_node().
set_type(#occi_node{}=Node, Type) ->
    Node#occi_node{type=Type}.

-spec add_child(occi_node(), uri() | occi_node()) -> occi_node().
add_child(#occi_node{type=dir, data=C}=Node, #occi_node{type=dir}=Child) ->
    Node#occi_node{data=gb_sets:add(Child, C)};
add_child(#occi_node{type=dir, data=C}=Node, #uri{}=Child) ->
    Node#occi_node{data=gb_sets:add(Child, C)}.

-spec set_children(occi_node(), term()) -> occi_node().
set_children(#occi_node{type=dir}=Node, Children) ->
    Node#occi_node{data=Children}.

-spec del_children(occi_node(), [uri() | occi_node()]) -> occi_node().
del_children(#occi_node{type=dir, data=C}=Node, Children) when is_list(Children) ->
    Node#occi_node{data=gb_sets:subtract(C, gb_sets:from_list(Children))}.

-spec get_parent(occi_node()) -> uri().
get_parent(#occi_node{id=Id}) ->
    occi_uri:get_parent(Id).

-spec set_parent(occi_node()) -> occi_node().
set_parent(#occi_node{id=Id}=Node) ->
    Node#occi_node{parent=occi_uri:get_parent(Id)}.

-spec has_children(occi_node()) -> boolean().
has_children(#occi_node{type=dir, data=Children}) ->
    not gb_sets:is_empty(Children);
has_children(_) ->
    false.

-spec get_data(occi_node()) -> term().
get_data(#occi_node{data=Data}) ->
    Data.

-spec set_data(occi_node(), term()) -> occi_node().
set_data(#occi_node{type=occi_resource}=Node, #occi_resource{}=Data) ->
    Node#occi_node{data=Data};
set_data(#occi_node{type=occi_link}=Node, #occi_link{}=Data) ->
    Node#occi_node{data=Data};
set_data(#occi_node{type=occi_mixin}=Node, #occi_mixin{}=Data) ->
    Node#occi_node{data=Data};
set_data(#occi_node{type=occi_user_mixin}=Node, #occi_mixin{}=Data) ->
    Node#occi_node{data=Data};
set_data(#occi_node{type=Type}, _) ->
    throw({invalid_data_type, Type}).

-spec add_prefix(occi_node(), list()) -> occi_node().
add_prefix(#occi_node{type=dir, id=#uri{}=Id, data=Data}=Node, Prefix) when is_list(Prefix) ->
    Children = gb_sets:fold(fun (#occi_node{}=Child, Acc) ->
				    gb_sets:add(occi_node:add_prefix(Child, Prefix), Acc);
				(#uri{}=Child, Acc) ->
				    gb_sets:add(occi_uri:add_prefix(Child, Prefix), Acc)
			    end, gb_sets:new(), Data),
    Node#occi_node{id=occi_uri:add_prefix(Id, Prefix), data=Children};

add_prefix(#occi_node{id=#uri{}=Id, objid=ObjId, data=Data}=Node, Prefix) when is_list(Prefix) ->
    N = Node#occi_node{id=occi_uri:add_prefix(Id, Prefix)},
    N2 = case ObjId of
	     #uri{}=Uri ->
		 N#occi_node{objid=occi_uri:add_prefix(Uri, Prefix)};
	     _ -> N
	 end,
    N2#occi_node{data=case Data of
			  #occi_resource{}=R -> occi_resource:add_prefix(R, Prefix);
			  #occi_link{}=L -> occi_link:add_prefix(L, Prefix);
			  #occi_mixin{}=M -> occi_mixin:add_prefix(M, Prefix);
			  #occi_collection{}=C -> occi_collection:add_prefix(C, Prefix);
			  _ -> Data
		      end}.

-spec rm_prefix(occi_node(), list()) -> occi_node().
rm_prefix(#occi_node{type=dir, id=#uri{}=Id, data=Data}=Node, Prefix) when is_list(Prefix) ->
    Children = gb_sets:fold(fun (#occi_node{}=Child, Acc) ->
				    gb_sets:add(occi_node:rm_prefix(Child, Prefix), Acc);
				(#uri{}=Child, Acc) ->
				    gb_sets:add(occi_uri:rm_prefix(Child, Prefix), Acc)
			    end, gb_sets:new(), Data),
    Node#occi_node{id=occi_uri:rm_prefix(Id, Prefix), data=Children};

rm_prefix(#occi_node{id=#uri{}=Id, objid=ObjId, data=Data}=Node, Prefix) when is_list(Prefix) -> 
    N = Node#occi_node{id=occi_uri:rm_prefix(Id, Prefix)},
    N2 = case ObjId of
	     #uri{}=Uri ->
		 N#occi_node{objid=occi_uri:rm_prefix(Uri, Prefix)};
	     _ -> N
	 end,
    N2#occi_node{data=case Data of
			  #occi_resource{}=R -> occi_resource:rm_prefix(R, Prefix);
			  #occi_link{}=L -> occi_link:rm_prefix(L, Prefix);
			  #occi_mixin{}=M -> occi_mixin:rm_prefix(M, Prefix);
			  #occi_collection{}=C -> occi_collection:rm_prefix(C, Prefix);
			  _ -> Data
		      end}.
