%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013-2015, Jean Parpaillon
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
%%% Created :  1 Jul 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_backend_mnesia).

-behaviour(occi_backend).

-include_lib("stdlib/include/qlc.hrl").
-include("occi.hrl").
-include("occi_log.hrl").

%% occi_backend callbacks
-export([init/1,
	 terminate/1]).
-export([update/2,
	 save/2,
	 delete/2,
	 find/2,
	 load/3,
	 action/3]).

-record(state, {}).

%%%===================================================================
%%% occi_backend callbacks
%%%===================================================================
init(#occi_backend{opts=Opts}) ->
    application:load(occi_backend_mnesia),
    init_schema(Opts).


terminate(#state{}) ->
    application:unload(occi_backend_mnesia),
    ok.

save(State, #occi_node{}=Obj) ->
    ?info("[~p] save(~p)~n", [?MODULE, Obj#occi_node.id]),
    case mnesia:transaction(fun () -> save_t(Obj) end) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.

delete(State, #occi_node{}=Obj) ->
    ?info("[~p] delete(~p)~n", [?MODULE, Obj#occi_node.id]),
    case mnesia:transaction(fun () -> del_node_t(Obj) end) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.

update(State, #occi_node{}=Node) ->
    ?info("[~p] update(~p)~n", [?MODULE, Node#occi_node.id]),
    case mnesia:transaction(fun () -> update_t(Node) end) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.    

find(State, #occi_node{}=Obj) ->
    ?info("[~p] find(~p)~n", [?MODULE, Obj#occi_node.id]),
    try mnesia:activity(async_dirty, fun find_node_t/1, [Obj]) of
	Res -> {{ok, Res}, State}
    catch exit:Reason -> {{error, Reason}, State}
    end.

load(State, #occi_node{}=Req, Opts) ->
    ?info("[~p] load(~p, ~p)~n", [?MODULE, Req#occi_node.id, Opts]),
    try mnesia:activity(async_dirty, fun load_node_t/2, [Req, Opts]) of
	Res -> {{ok, Res}, State}
    catch exit:Reason -> {{error, Reason}, State}
    end.

action(State, #uri{}=Id, #occi_action{}=A) ->
    ?info("[~p] action(~p, ~p)~n", [?MODULE, Id, A]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
find_node_t(#occi_node{type=capabilities, objid=#occi_cid{}}=N) ->
    mnesia:match_object(N);

find_node_t(#occi_node{type=capabilities}=Caps) ->
    case mnesia:match_object(Caps)  of
	[] -> [occi_capabilities:new()];
	[Mixins] ->
	    F = fun (#occi_node{data=#occi_mixin{}=M}, Acc) ->
			occi_capabilities:add_mixin(Acc, M)
		end,
	    lists:foldl(F, Caps, Mixins)
    end;

find_node_t(#occi_node{id=Id}) ->
    case mnesia:wread({occi_node, Id}) of
	[] -> [];
	[Node] -> [Node#occi_node{data=undefined}]
    end.


load_node_t(#occi_node{objid=Id, type=occi_collection}=Node, 
	    #occi_store_opts{node_f=NF, entity_f=EF, deep=D, limit=L, marker=M}) ->
    QH = get_collection_qh(Id, NF, EF, D),
    Node#occi_node{data=load_collection_t(Id, QH, L, parse_marker(M))};

load_node_t(#occi_node{type=occi_resource, objid=Id}=Node, _Opts) ->
    Node2 = load_object_t(Node, occi_resource, Id),
    load_resource_node_t(Node2);

load_node_t(#occi_node{type=occi_link, objid=Id}=Node, _Opts) ->
    load_object_t(Node, occi_link, Id).


load_object_t(Node, Type, Id) ->
    case mnesia:wread({Type, Id}) of
	[] ->
	    mnesia:abort({unknown_object, {Type, Id}});
	[Data] ->
	    Node#occi_node{data=Data}
    end.


load_collection_t(Id, QH, -1, 0) ->
    occi_collection:new(Id, qlc:e(QH));

load_collection_t(Id, QH, L, M) ->
    QC = qlc:cursor(QH),
    case get_range_t(QC, M, L) of
	{Ret, 0, Count} ->
	    qlc:delete_cursor(QC),
	    #occi_collection{id=Id, range={M, Count, 0},
			     entities=ordsets:from_list(Ret)};
	{Ret, M2, Count} ->
	    qlc:delete_cursor(QC),
	    #occi_collection{id=Id, range={M, Count, 0}, 
			     marker=integer_to_binary(M2), 
			     entities=ordsets:from_list(Ret)}
    end.


get_range_t(QC, 0, Limit) ->
    Ret = qlc:next_answers(QC, Limit),
    Count = length(Ret),
    if Count < Limit ->
	    {Ret, 0, Count};
       true ->
	    {Ret, Limit+1, Count}
    end;

get_range_t(QC, Start, Limit) ->
    case get_range_t(QC, 0, Start-1) of
	{_, 0, _} ->
	    {[], 0, 0};
	{_, NewStart, _} ->
	    Ret = qlc:next_answers(QC, Limit),
	    Count = length(Ret),
	    if Count < Limit ->
		    {Ret, 0, Count};
	       true ->
		    {Ret, NewStart+Limit, Count}
	    end
    end.
	

load_resource_node_t(#occi_node{data=#occi_resource{links=OrigLinks}=Res}=Node) ->
    Links = sets:fold(fun (#uri{}=LinkId, Acc) ->
			      sets:add_element(LinkId, Acc);
			  ({inline, Id}, Acc) ->
			      sets:add_element(get_inline_link_t(Id), Acc)
		      end, sets:new(), OrigLinks),
    Node#occi_node{data=Res#occi_resource{links=Links}}.


save_t(#occi_node{id=Id, type=occi_resource, data=Res}=Node) ->
    ObjId = make_ref(),
    { Node2, LinksNodes } = 
	get_resource_links_t(Node#occi_node{objid=ObjId, data=occi_resource:set_id(Res, ObjId)}),
    save_entity_t(Id, Node2#occi_node.data),
    save_resource_links_t(LinksNodes),
    save_node_t(Node2);

save_t(#occi_node{id=Id, type=occi_link, data=Link}=Node) ->
    ObjId = make_ref(),
    save_entity_t(Id, occi_link:set_id(Link, ObjId)),
    save_node_t(Node#occi_node{objid=ObjId});

save_t(#occi_node{type=occi_collection, data=Coll}) ->
    save_collection_t(Coll).


save_resource_links_t([]) ->
    ok;

save_resource_links_t([#occi_node{id=Id, data=Link}=Node | Nodes]) ->
    % Save link node but don't put it in parent collection (hidden path)
    save_resource_link_t(Id, Link),
    mnesia:write(Node#occi_node{data=undefined}),
    save_resource_links_t(Nodes).


save_resource_link_t(Id, Link) ->
    add_link_t(Id, occi_link:get_target(Link)),
    mnesia:write(Link),
    sets:fold(fun (#occi_cid{}=MixinId, _) ->
		      add_to_collection_t(MixinId, [Id])
	      end, ok, occi_link:get_mixins(Link)),
    ok.


save_collection_t(#occi_collection{}=Coll) ->
    lists:foreach(fun (#uri{}=Id) ->
			  case check_entity_t(Id) of
			      true -> ok;
			      false -> mnesia:abort({unknown_entity, Id})
			  end
		  end, occi_collection:get_entities(Coll)),
    mnesia:write(Coll).

save_entity_t(Id, #occi_resource{}=Res) ->
    mnesia:write(Res),
    sets:fold(fun (#occi_cid{}=MixinId, _) ->
		      add_to_collection_t(MixinId, [Id])
	      end, ok, occi_resource:get_mixins(Res)),
    ok;

save_entity_t(Id, #occi_link{}=Link) ->
    add_link_t(Id, occi_link:get_source(Link)),
    add_link_t(Id, occi_link:get_target(Link)),
    mnesia:write(Link),
    sets:fold(fun (#occi_cid{}=MixinId, _) ->
		      add_to_collection_t(MixinId, [Id])
	      end, ok, occi_link:get_mixins(Link)),
    ok.

add_link_t(LinkId, Id) ->
    case get_resource_t(Id) of
	false ->
	    mnesia:abort({unknown_resource, Id});
	{ok, Res} ->
	    mnesia:write(occi_resource:add_link(Res, LinkId))
    end.

save_node_t(#occi_node{id=Id}=Node) ->
    case mnesia:wread({occi_node, Id}) of
	[_] -> 	
	    mnesia:write(Node#occi_node{data=undefined});
	[] -> 
	    mnesia:write(Node#occi_node{data=undefined}),
	    add_to_collection_t(occi_uri:get_parent(Id), {entity, Id})
    end.

update_t(#occi_node{type=capabilities}=Node) ->
    mnesia:write(Node);

update_t(#occi_node{type=occi_collection, data=#occi_collection{id=#occi_cid{}=Cid}=Coll}) ->
    Mixin = get_category_t(Cid),
    Entities = occi_collection:get_entities(Coll),
    lists:foreach(fun (#uri{}=Id) ->
			  case get_entity_t(Id) of
			      false -> 
				  mnesia:abort({unknown_entity, Id});
			      {ok, Entity} ->
				  mnesia:write(occi_entity:add_mixin(Entity, Mixin))
			  end
    		  end, Entities),
    add_to_collection_t(Cid, Entities);

update_t(#occi_node{id=Id, type=occi_resource, data=Res}) ->
    save_entity_t(Id, Res);

update_t(#occi_node{id=Id, type=occi_link, data=Res}) ->
    save_entity_t(Id, Res).

add_to_collection_t(none, _) ->
    ok;

add_to_collection_t(#uri{path=Path}=Parent, Child) ->
    case mnesia:wread({occi_node, Parent}) of
	[] ->
	    mnesia:write(occi_node:new(Parent, occi_collection:new(Parent, [Child])));
	[#occi_node{type=occi_collection, data=#occi_collection{}=Coll}=Node] ->
	    mnesia:write(Node#occi_node{data=occi_collection:add_entity(Coll, Child)});
	[#occi_node{}] ->
	    mnesia:abort({unknown_collection, Parent})
    end,
    if Path == [] -> ok;
       true -> add_to_collection_t(occi_uri:get_parent(Parent), {collection, Parent})
    end;

add_to_collection_t(#occi_cid{class=mixin}=Cid, Uris) ->
    case mnesia:wread({occi_collection, Cid}) of
	[#occi_collection{}=C] ->
	    mnesia:write(occi_collection:add_entities(C, Uris));
	[] ->
	    mnesia:write(occi_collection:new(Cid, Uris))
    end.

del_node_t(#occi_node{id=Id, type=capabilities, data=#occi_mixin{}=Mixin}) ->
    del_mixin_t(Mixin),
    mnesia:delete({occi_node, Id});

del_node_t(#occi_node{id=Id, type=occi_collection, objid=#uri{}, data=Col}=Node) ->
    lists:foreach(fun (#uri{}=ChildId) ->
			  case mnesia:wread({occi_node, ChildId}) of
			      [] -> 
				  ignore;
			      [#occi_node{}=Child] ->
				  del_node_t(Child)
			  end
		  end, occi_collection:get_entities(Col)),
    del_from_collection_t(occi_node:get_parent(Node), Id),
    mnesia:delete({occi_node, Id});

del_node_t(#occi_node{type=occi_collection, objid=#occi_cid{}, data=Col}) ->
    del_collection_t(Col);

del_node_t(#occi_node{type=occi_resource, data=undefined}=Node) ->
    del_node_t(load_node_t(Node, []));

del_node_t(#occi_node{id=Id, type=occi_resource, data=Res}=Node) ->
    del_entity_t(Res),
    del_from_collection_t(occi_node:get_parent(Node), Id),
    mnesia:delete({occi_node, Id});

del_node_t(#occi_node{type=occi_link, data=undefined}=Node) ->
    del_node_t(load_node_t(Node, []));

del_node_t(#occi_node{id=Id, type=occi_link, data=Res}=Node) ->
    del_entity_t(Res),
    del_from_collection_t(occi_node:get_parent(Node), Id),
    mnesia:delete({occi_node, Id}).

del_entity_t(#occi_resource{id=Id}=Res) ->
    sets:fold(fun (MixinId, _) ->
		      del_from_collection_t(MixinId, [Id])
		  end, ok, occi_resource:get_mixins(Res)),
    mnesia:delete({occi_resource, Id});

del_entity_t(#occi_link{id=Id}=Link) ->
    sets:fold(fun (MixinId, _) ->
		      del_from_collection_t(MixinId, [Id])
	      end, ok, occi_link:get_mixins(Link)),
    mnesia:delete({occi_link, Id}).

del_from_collection_t(#uri{}=Id, Uri) ->
    case mnesia:wread({occi_node, Id}) of
	[] ->
	    mnesia:abort({unkown_collection, Id});
	[#occi_node{type=occi_collection, data=Col}=Node] ->
	    Col1 = occi_collection:del_entity(Col, {entity, Uri}),
	    case occi_collection:is_empty(Col1) of
		true ->
		    mnesia:delete({occi_node, Id});
		false ->
		    mnesia:write(Node#occi_node{data=Col1})
	    end;
	_ ->
	    mnesia:abort({not_a_collection, Id})
    end;

del_from_collection_t(#occi_cid{class=mixin}=Cid, Uris) ->
    case mnesia:wread({occi_collection, Cid}) of
	[#occi_collection{}=C] ->
	    mnesia:write(occi_collection:del_entities(C, Uris));
	[] ->
	    mnesia:abort({unknown_collection, Cid})
    end.

del_collection_t(#occi_collection{id=#occi_cid{class=kind}}=Coll) ->
    F = fun (#uri{}=Id, Acc) ->
		case mnesia:wread({occi_node, Id}) of
		    [#occi_node{type=Type, objid=ObjId}=N] ->
			Node = load_node_t(N, []),
			mnesia:delete({Type, ObjId}),
			del_entity_t(Node#occi_node.data),
			mnesia:delete({occi_node, Id}),
			sets:fold(fun (MixinId, Acc2) ->
					  dict:append(MixinId, Id, Acc2)
				  end, 
				  dict:append(occi_node:get_parent(Node), Id, Acc),
				  occi_entity:get_mixins(Node#occi_node.data));
		    [] ->
			mnesia:abort({unknown_object, Id})
		end
	end,
    Uris = occi_collection:get_entities(Coll),
    Colls = lists:foldl(F, dict:new(), Uris),
    dict:map(fun (Key, Values) ->
		     del_from_collection_t(Key, Values)
	     end, Colls),
    ok;

del_collection_t(#occi_collection{id=#occi_cid{class=mixin}=Cid}=Coll) ->
    Mixin = get_category_t(Cid),
    del_collection_t(Coll, Mixin).

del_collection_t(#occi_collection{id=#occi_cid{class=mixin}=Cid}=Coll, #occi_mixin{}=Mixin) ->
    Entities = occi_collection:get_entities(Coll),
    del_mixin_from_entities_t(Entities, Mixin),
    del_from_collection_t(Cid, Entities).

del_full_collection_t(#occi_collection{id=#occi_cid{class=mixin}=Cid}=Coll, #occi_mixin{}=Mixin) ->
    Entities = occi_collection:get_entities(Coll),
    del_mixin_from_entities_t(Entities, Mixin),
    mnesia:delete({occi_collection, Cid}).


del_mixin_from_entities_t(Entities, Mixin) ->
    lists:foreach(fun (Id) ->
			  case mnesia:wread({occi_node, Id}) of
			      [N] ->
				  #occi_node{data=Entity} = load_node_t(N, []),
				  mnesia:write(occi_entity:del_mixin(Entity, Mixin));
			      [] ->
				  mnesia:abort({unknown_object, Id})
			  end
		  end, Entities).


del_mixin_t(#occi_mixin{id=Cid}=Mixin) ->
    case mnesia:wread({occi_collection, Cid}) of
	[#occi_collection{}=Coll] ->
	    del_full_collection_t(Coll, Mixin);
	[] ->
	    ok
    end.


get_category_parent_t(#occi_cid{}=Cid) ->
    case get_category_t(Cid) of
	#occi_kind{parent=?cid_resource} -> occi_resource;
	#occi_kind{parent=?cid_link} -> occi_link;
	_ -> mnesia:abort({invalid_kind, Cid})
    end.


get_category_t(#occi_cid{}=Cid) ->
    case occi_store:get(Cid) of
	{ok, #occi_mixin{}=Mixin} -> Mixin;
	{ok, #occi_kind{}=Kind} -> Kind;
	_ -> mnesia:abort({invalid_cid, Cid})
    end.


check_entity_t(#uri{}=Id) ->
    case occi_uri:is_rel(Id) of
	true ->
	    case mnesia:wread({occi_resource, Id}) of
		[] ->
		    case mnesia:wread({occi_link, Id}) of
			[] -> false;
			_ -> true
		    end;
		_ -> true
	    end;
	false ->
	    case occi_store:find(#occi_node{id=Id, _='_'}) of
		{ok, []} -> false;
		{ok, [_]} -> true
	    end
    end.


get_resource_t(Id) ->
    case occi_uri:is_rel(Id) of
	true ->
	    case mnesia:wread({occi_node, Id}) of
		[] -> false;
		[#occi_node{objid=ObjId}] ->
		    case mnesia:wread({occi_resource, ObjId}) of
			[] -> false;
			[Res] -> {ok, Res}
		    end
	    end;
	false ->
	    case occi_store:find(#occi_node{id=Id, _='_'}) of
		{ok, []} -> false;
		{ok, [#occi_resource{}=Res]} -> Res
	    end
    end.


get_entity_t(Id) ->
    case mnesia:wread({occi_node, Id}) of
	[] -> false;
	[#occi_node{objid=ObjId}] ->
	    case mnesia:wread({occi_resource, ObjId}) of
		[] -> 
		    case mnesia:wread({occi_link, ObjId}) of
			[] -> false;
			[Link] -> {ok, Link}
		    end;
		[Res] -> {ok, Res}
	    end
    end.


get_resource_links_t(#occi_node{id=ResId, owner=Owner, data=Res}=Node) ->
    Links = occi_resource:get_links(Res),
    F = fun (#uri{}=Link, {AccUris, AccNodes}) ->
		{ sets:add_element(Link, AccUris), AccNodes };
	    (#occi_link{}=Link, {AccUris, AccNodes}) ->
		Id = create_link_id(ResId),
		LinkNode = occi_node:new(Link#occi_link{id=Id, source=ResId}, Owner),
		{ sets:add_element({inline, Id}, AccUris), [ LinkNode | AccNodes ] }
	end,
    {LinksIds, LinksNodes} = lists:foldl(F, {sets:new(), []}, Links),
    { Node#occi_node{data=Res#occi_resource{links=LinksIds}}, LinksNodes }.


create_link_id(#uri{path=Path}=Id) ->
    Id#uri{path=Path ++ "/." ++ uuid:to_string(uuid:uuid4())}.


get_inline_link_t(Id) ->
    case mnesia:wread({occi_node, Id}) of
	[] -> 
	    mnesia:abort({unknown_node, Id});
	[#occi_node{objid=ObjId}] ->
	    case mnesia:wread({occi_link, ObjId}) of
		[] ->
		    mnesia:abort({unknown_object, ObjId});
		[Link] ->
		    Link#occi_link{id=Id}
	    end
    end.


get_collection_qh(#occi_cid{class=kind}=Cid, NodeFilters, EntityFilters, true) ->
    EntityType = get_category_parent_t(Cid),
    Q1 = qlc:q([ N#occi_node{data=E} || N <- mnesia:table(occi_node), 
					E <- mnesia:table(EntityType),
					N#occi_node.type =:= EntityType,
					element(3, E) =:= Cid,
					N#occi_node.objid =:= element(2, E),
					apply_node_filters(NodeFilters, N),
					apply_entity_filters(EntityFilters, E)]),
    case EntityType of
	occi_resource ->
	    qlc:fold(fun (E, Acc) -> 
			     [load_resource_node_t(E) | Acc] 
		     end, [], Q1);
	occi_link ->
	    Q1
    end;

get_collection_qh(#occi_cid{class=kind}=Cid, NodeFilters, EntityFilters, false) ->
    EntityType = get_category_parent_t(Cid),
    qlc:q([ element(2, N) || N <- mnesia:table(occi_node), 
			     E <- mnesia:table(EntityType),
			     N#occi_node.type =:= EntityType,
			     element(3, E) =:= Cid,
			     N#occi_node.objid =:= element(2, E),
			     apply_node_filters(NodeFilters, N),
			     apply_entity_filters(EntityFilters, E)]);

get_collection_qh(#occi_cid{class=mixin}=Cid, NodeFilters, EntityFilters, true) ->
    Q1 = qlc:q([ N#occi_node{data=R} || N <- mnesia:table(occi_node), 
					R <- mnesia:table(occi_resource),
					N#occi_node.objid =:= element(2, R),
					N#occi_node.type =:= occi_resource,
					occi_resource:has_category(R, Cid),
					apply_node_filters(NodeFilters, N),
					apply_entity_filters(EntityFilters, R)]),
    Q2 = qlc:fold(fun (E, Acc) -> [load_resource_node_t(E) | Acc] end, [], Q1),
    Q3 = qlc:q([ N#occi_node{data=L} || N <- mnesia:table(occi_node), 
					L <- mnesia:table(occi_link),
					N#occi_node.objid =:= element(2, L),
					N#occi_node.type =:= occi_link,
					occi_link:has_category(L, Cid),
					apply_node_filters(NodeFilters, N),
					apply_entity_filters(EntityFilters, L)]),
    qlc:append(Q2, Q3);

get_collection_qh(#occi_cid{class=mixin}=Cid, NodeFilters, EntityFilters, false) ->
    Q1 = qlc:q([ element(2, N) || N <- mnesia:table(occi_node), 
				  R <- mnesia:table(occi_resource),
				  N#occi_node.objid =:= element(2, R),
				  N#occi_node.type =:= occi_resource,
				  occi_resource:has_category(R, Cid),
				  apply_node_filters(NodeFilters, N),
				  apply_entity_filters(EntityFilters, R)]),
    Q2 = qlc:q([ element(2, L) || N <- mnesia:table(occi_node), 
				  L <- mnesia:table(occi_link),
				  N#occi_node.objid =:= element(2, L),
				  N#occi_node.type =:= occi_link,
				  occi_link:has_category(L, Cid),
				  apply_node_filters(NodeFilters, N),
				  apply_entity_filters(EntityFilters, L)]),
    qlc:append(Q1, Q2);

get_collection_qh(#uri{}=CollId, NodeFilters, EntityFilters, Deep) ->
    case mnesia:wread({occi_node, CollId}) of
	[] ->
	    mnesia:abort({unknown_node, CollId});
	[#occi_node{data=#occi_collection{entities=Entities}}] ->
	    F = fun ({entity, Id}, Acc) -> 
			case get_filtered_node_t(Id, NodeFilters, EntityFilters, Deep) of
			    none -> Acc;
			    Node -> 
				case Deep of 
				    true -> [Acc, Node];
				    false -> [Acc, Node#occi_node.id]
				end
			end;
		    ({collection, Id}, Acc) ->
			[Acc, get_collection_qh(Id, NodeFilters, EntityFilters, Deep) ]
		end,
	    lists:flatten(ordsets:fold(F, [], Entities))
    end.


get_filtered_node_t(Id, NodeFilters, [], false) ->
    case mnesia:wread({occi_node, Id}) of
	[] -> mnesia:abort({unknown_node, Id});
	[Node] ->
	    case apply_node_filters(NodeFilters, Node) of
		true -> Node;
		false -> none
	    end
    end;

get_filtered_node_t(Id, NodeFilters, EntityFilters, _) ->
    case mnesia:wread({occi_node, Id}) of
	[] -> mnesia:abort({unknown_node, Id});
	[Node] ->
	    case apply_node_filters(NodeFilters, Node) of
		true -> get_filtered_entity_t(Node, EntityFilters);
		false -> none
	    end
    end.    


get_filtered_entity_t(#occi_node{type=Type, objid=ObjId}=Node, EntityFilters) ->
    case mnesia:wread({Type, ObjId}) of
	[] -> mnesia:abort({unknown_entity, ObjId});
	[#occi_link{}=L] ->
	    case apply_entity_filters(EntityFilters, L) of
		true -> Node#occi_node{data=L};
		false -> none
	    end;
	[#occi_resource{}=R] ->
	    case apply_entity_filters(EntityFilters, R) of
		true -> load_resource_node_t(Node#occi_node{data=R});
		false -> none
	    end
    end.


apply_node_filters([], _) ->
    true;

apply_node_filters([Fun | Rest], N) when is_function(Fun) ->
    case Fun(N) of
	true -> apply_node_filters(Rest, N);
	false -> false
    end.
	    

apply_entity_filters([], _) ->
    true;

apply_entity_filters([#occi_cid{}=Cid | Rest], E) ->
    case occi_entity:has_entity(E, Cid) of
	true -> apply_entity_filters(Rest, E);
	false -> false
    end;

apply_entity_filters([{Op, Name, Match} | Rest], E) ->
    case occi_entity:match_attr(E, Name, {Op, Match}) of
	true -> apply_entity_filters(Rest, E);
	false -> false
    end.


parse_marker(Bin) ->
    try binary_to_integer(Bin) of
	I -> I
    catch _:_ -> 0
    end.


init_schema(Opts) ->
    application:load(mnesia),
    case application:get_env(mnesia, dir, "") of
	"" ->
	    init_db(Opts);
	Path ->
	    case filelib:is_file(filename:join([Path, "schema.DAT"])) of
		true ->
		    ?debug("mnesia schema already exists on this node", []),
		    init_db(Opts);
		false ->
		    ?debug("Creating mnesia schema on ~s", [Path]),
		    case mnesia:create_schema([node()]) of
			ok ->
			    init_db(Opts);
			{error, {_, {already_exists,_}}} ->
			    init_db(Opts);
			{error, Err} ->
			    ?error("Error creating mnesia schema on node ~p: ~p", [node(), Err]),
			    {error, Err}
		    end
	    end
    end.

init_db(Opts) ->
    application:start(mnesia, permanent),
    Tables = [{occi_collection, [{disc_copies, [node()]},
				 {attributes, record_info(fields, occi_collection)}]},
	      {occi_resource, [{disc_copies, [node()]},
			       {attributes, record_info(fields, occi_resource)},
			       {index, [#occi_resource.cid]}]},
	      {occi_link, [{disc_copies, [node()]},
			   {attributes, record_info(fields, occi_link)},
			   {index, [#occi_link.cid]}]},
	      {occi_mixin, [{disc_copies, [node()]},
			    {attributes, record_info(fields, occi_mixin)}]},
	      {occi_node, [{disc_copies, [node()]}, 
			   {attributes, record_info(fields, occi_node)},
			   {index, [#occi_node.objid]},
			   {type, 'ordered_set'}]},
	      {occi_user, [{disc_copies, [node()]},
			   {attributes, record_info(fields, occi_user)}]}],
    case init_tables(Tables) of
	ok ->
	    mnesia:wait_for_tables([occi_collection, occi_resource, occi_link,
				    occi_mixin, occi_node, occi_user], 5000),
	    init_backend(Opts);
	{error, Err} ->
	    ?error("Error creating mnesia tables: ~p", [Err]),
	    {error, Err}
    end.

init_tables([]) ->
    ok;
init_tables([{Name, TabDef} | Tables]) ->
    case mnesia:create_table(Name, TabDef) of
	{atomic, ok} ->
	    ?debug("mnesia: created table: ~p~n", [Name]),
	    init_tables(Tables);
	{aborted, {already_exists, Name}} ->
	    ?debug("mnesia: table ~p already exists~n", [Name]),
	    init_tables(Tables);
	{aborted, Reason} ->
	    {error, Reason}
    end.

init_backend(Opts) ->
    Schemas = lists:map(fun ({path, _Path}=S) -> 
				S;
			    ({priv_dir, Path}) -> 
				Fullpath = filename:join([code:priv_dir(occi_backend_mnesia), Path]),
				{path, Fullpath}
			end, 
		       proplists:get_value(schemas, Opts, []) 
			++ application:get_env(occi_backend_mnesia, schemas, [])),
    F = fun() -> mnesia:match_object(#occi_node{type=capabilities, _='_'}) end,
    case mnesia:transaction(F) of
	{atomic, []} -> 
	    {ok, [{schemas, Schemas}], #state{}};
	{atomic, MixinsNodes} ->
	    Mixins = [ Mixin || #occi_node{data=Mixin} <- MixinsNodes],
	    {ok, [{schemas, lists:flatten([Schemas, Mixins])}], #state{}};
	{aborted, Reason} ->
	    {error, Reason}
    end.
