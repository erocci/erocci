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
%%% Created :  1 Jul 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_backend_mnesia).
-compile({parse_transform, lager_transform}).

-behaviour(occi_backend).

-include("occi.hrl").

%% occi_backend callbacks
-export([init/1,
	 terminate/1]).
-export([update/2,
	 save/2,
	 delete/2,
	 find/2,
	 load/2]).

-record(state, {}).

%%%===================================================================
%%% occi_backend callbacks
%%%===================================================================
init(_) ->
    mnesia:create_table(occi_collection,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, occi_collection)}]),
    mnesia:create_table(occi_resource,
		       [{disc_copies, [node()]},
			{attributes, record_info(fields, occi_resource)}]),
    mnesia:create_table(occi_link,
		       [{disc_copies, [node()]},
			{attributes, record_info(fields, occi_link)}]),
    mnesia:create_table(occi_mixin,
		       [{disc_copies, [node()]},
			{attributes, record_info(fields, occi_mixin)}]),
    mnesia:create_table(occi_node,
		       [{disc_copies, [node()]},
			{attributes, record_info(fields, occi_node)}]),
    mnesia:wait_for_tables([occi_resource, occi_category], infinite),
    {ok, #state{}}.

terminate(#state{}) ->
    ok.

save(#occi_node{}=Node, State) ->
    case mnesia:transaction(fun () -> save_t(Node) end) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.

delete(#occi_node{}=Node, State) ->
    case mnesia:transaction(fun () -> del_node_t(Node) end) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.

update(#occi_node{}=Node, State) ->
    case mnesia:transaction(fun () -> update_t(Node) end) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.    

find(#occi_node{}=Req, State) ->
    case mnesia:transaction(fun () ->
				    find_node_t(Req)
			    end) of
	{atomic, Res} -> 
	    {{ok, Res}, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.

load(#occi_node{}=Req, State) ->
    case mnesia:transaction(fun () ->
				    load_node_t(Req)
			    end) of
	{atomic, Res} -> 
	    {{ok, Res}, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
find_node_t(#occi_node{type=occi_query}) ->
    mnesia:match_object(#occi_mixin{_='_'});

find_node_t(#occi_node{type=occi_user_mixin}=Node) ->
    mnesia:match_object(Node);

find_node_t(#occi_node{type=occi_collection}=Node) ->
    mnesia:match_object(Node);

find_node_t(#occi_node{id=Id}) ->
    case mnesia:wread({occi_node, Id}) of
	[] ->
	    [];
	[#occi_node{}=Node] ->
	    [Node]
    end.

load_node_t(#occi_node{type=occi_collection, objid=Id}=Node) ->
    case mnesia:wread({occi_collection, Id}) of
	[] ->
	    Node#occi_node{data=occi_collection:new(Id)};
	[Coll] ->
	    Node#occi_node{data=Coll}
    end;

load_node_t(#occi_node{type=occi_resource, objid=Id}=Node) ->
    load_object_t(Node, occi_resource, Id);

load_node_t(#occi_node{type=occi_link, objid=Id}=Node) ->
    load_object_t(Node, occi_link, Id);

load_node_t(#occi_node{type=dir}=Node) ->
    load_dir_t(Node).

load_dir_t(#occi_node{data=Children}=Node) ->
    Children2 = gb_sets:fold(fun (ChildId, Acc) ->
				     case mnesia:wread({occi_node, ChildId}) of
					 [] ->
					     mnesia:abort({unknown_node, ChildId});
					 [#occi_node{type=dir}=Child] ->
					     gb_sets:add(load_dir_t(Child), Acc);
					 [#occi_node{}=Child] ->
					     gb_sets:add(Child, Acc)
				     end
			     end, gb_sets:new(), Children),
    Node#occi_node{data=Children2}.

load_object_t(Node, Type, Id) ->
    case mnesia:wread({Type, Id}) of
	[] ->
	    mnesia:abort({unknown_object, {Type, Id}});
	[Data] ->
	    Node#occi_node{data=Data}
    end.

save_t(#occi_node{type=occi_resource, data=Res}=Node) ->
    save_entity_t(Res),
    save_node_t(Node);

save_t(#occi_node{type=occi_link, data=Res}=Node) ->
    save_entity_t(Res),
    save_node_t(Node);

save_t(#occi_node{type=occi_user_mixin, data=#occi_mixin{}=Mixin}=Node) ->
    mnesia:write(Mixin),
    save_node_t(Node#occi_node{type=occi_collection});

save_t(#occi_node{type=occi_collection, data=Coll}) ->
    save_collection_t(Coll).

save_collection_t(#occi_collection{}=Coll) ->
    lists:foreach(fun (#uri{}=Id) ->
			  case mnesia:wread({occi_resource, Id}) of
			      [] ->
				  case mnesia:wread({occi_link, Id}) of
				      [] -> mnesia:abort({no_such_entity, Id});
				      _ -> ok
				  end;
			      _ -> ok
			  end
		  end, occi_collection:get_entities(Coll)),
    mnesia:write(Coll).

save_entity_t(#occi_resource{}=Res) ->
    mnesia:write(Res),
    KindId = occi_resource:get_cid(Res),
    Uri = occi_resource:get_id(Res),
    add_to_collection_t(KindId, [Uri]),
    lists:foreach(fun (#occi_cid{}=MixinId) ->
			  add_to_collection_t(MixinId, [Uri])
		  end, occi_resource:get_mixins(Res)),
    ok;

save_entity_t(#occi_link{}=Link) ->
    add_link_t(occi_link:get_id(Link), occi_link:get_source(Link)),
    add_link_t(occi_link:get_id(Link), occi_link:get_target(Link)),
    mnesia:write(Link),
    KindId = occi_link:get_cid(Link),
    Uri = occi_link:get_id(Link),
    add_to_collection_t(KindId, [Uri]),
    lists:foreach(fun (#occi_cid{}=MixinId) ->
			  add_to_collection_t(MixinId, [Uri])
		  end, occi_link:get_mixins(Link)),
    ok.

add_link_t(LinkId, ResId) ->
    case mnesia:wread({occi_resource, ResId}) of
	[] ->
	    mnesia:abort({unknown_resource, ResId});
	[Res] ->
	    mnesia:write(occi_resource:add_link(Res, LinkId))
    end.

save_node_t(#occi_node{id=Id}=Node) ->
    case mnesia:wread({occi_node, Id}) of
	[_] -> 	
	    mnesia:write(Node#occi_node{data=undefined});
	[] -> 
	    mnesia:write(Node#occi_node{data=undefined}),
	    add_to_dir_t(occi_node:get_parent(Id), Id)
    end.

add_to_dir_t(#uri{path=Path}=Parent, #uri{}=Child) ->
    case mnesia:wread({occi_node, Parent}) of
	[] ->
	    Node = occi_node:new(Parent, dir),
	    mnesia:write(occi_node:add_child(Node, Child));
	[#occi_node{type=dir}=Node] ->
	    mnesia:write(occi_node:add_child(Node, Child));
	[#occi_node{}] ->
	    mnesia:abort({not_a_dir, Parent})
    end,
    if Path /= "/" ->
	    add_to_dir_t(occi_node:get_parent(Parent), Parent);
       true -> ok
    end.

update_t(#occi_node{type=occi_collection, data=#occi_collection{cid=Cid}=Coll}) ->
    Mixin = get_mixin_t(Cid),
    Entities = occi_collection:get_entities(Coll),
    lists:foreach(fun (#uri{}=Id) ->
    			  case mnesia:wread({occi_resource, Id}) of
    			      [#occi_resource{}=Res] ->
    				  mnesia:write(occi_resource:add_mixin(Res, Mixin));
    			      [] ->
    				  case mnesia:wread({occi_link, Id}) of
    				      [#occi_link{}=Link] ->
    					  mnesia:write(occi_link:add_mixin(Link, Mixin));
    				      [] -> 
    					  mnesia:abort({no_such_entity, Id})
    				  end
    			  end
    		  end, Entities),
    add_to_collection_t(Cid, Entities);

update_t(#occi_node{type=occi_resource, data=Res}) ->
    save_entity_t(Res).

add_to_collection_t(#occi_cid{class=kind}=Cid, Uris) ->
    case mnesia:wread({occi_collection, Cid}) of
	[#occi_collection{}=C] ->
	    mnesia:write(occi_collection:add_entities(C, Uris));
	[] ->
	    mnesia:write(occi_collection:new(Cid, Uris))
    end;

add_to_collection_t(#occi_cid{class=mixin}=Cid, Uris) ->
    case mnesia:wread({occi_collection, Cid}) of
	[#occi_collection{}=C] ->
	    mnesia:write(occi_collection:add_entities(C, Uris));
	[] ->
	    mnesia:write(occi_collection:new(Cid, Uris))
    end;

add_to_collection_t(#occi_cid{class=usermixin}=Cid, Uris) ->
    case mnesia:wread({occi_collection, Cid}) of
	[#occi_collection{}=C] ->
	    mnesia:write(occi_collection:add_entities(C, Uris));
	[] ->
	    mnesia:write(occi_collection:new(Cid, Uris))
    end.

del_node_t(#occi_node{type=occi_user_mixin, data=undefined}=Node) ->
    del_node_t(load_node_t(Node));

del_node_t(#occi_node{id=Id, type=occi_user_mixin, objid=Cid, data=Mixin}) ->
    del_mixin_t(Mixin),
    mnesia:delete({occi_mixin, Cid}),
    del_from_parent_node_t(occi_node:get_parent(Id), Id),
    mnesia:delete({occi_node, Id});

del_node_t(#occi_node{type=occi_collection, data=Coll}) ->
    del_collection_t(Coll);

del_node_t(#occi_node{id=Id, type=occi_resource, data=Res}) ->
    del_entity_t(Res),
    mnesia:delete({occi_node, Id});

del_node_t(#occi_node{id=Id, type=occi_link, data=Res}) ->
    del_entity_t(Res),
    mnesia:delete({occi_node, Id});

del_node_t(#occi_node{id=Id, type=dir, data=Children, parent=ParentId, recursive=true}) ->
    del_children_node_t(Children),
    del_from_parent_node_t(ParentId, Id),
    mnesia:delete({occi_node, Id});

del_node_t(#occi_node{id=Id, type=dir, data=Children, parent=undefined, recursive=true}) ->
    del_children_node_t(Children),
    mnesia:delete({occi_node, Id});

del_node_t(#occi_node{id=Id, parent=undefined}) ->
    mnesia:delete({occi_node, Id});

del_node_t(#occi_node{id=Id, parent=PP}) ->
    mnesia:delete({occi_node, Id}),
    del_from_parent_node_t(PP, Id).

del_children_node_t(Children) ->
    gb_sets:fold(fun (#occi_node{}=Child, _) ->
			 del_node_t(Child)
		 end, ok, Children).

del_from_parent_node_t(ParentId, Id) ->
    case mnesia:wread({occi_node, ParentId}) of
	[] ->
	    mnesia:abort({no_such_dir, ParentId});
	[#occi_node{type=dir}=Parent] ->
	    Parent1 = occi_node:del_children(Parent, [Id]),
	    case occi_node:has_children(Parent1) of
		false ->
		    mnesia:delete({occi_node, ParentId});
		true ->
		    mnesia:write(Parent1)
	    end;
	_ ->
	    mnesia:abort({not_a_dir, ParentId})
    end.    
    
del_entity_t(#occi_resource{id=Id, cid=Cid}=Res) ->
    del_from_collection_t(Cid, [Id]),
    lists:foreach(fun (MixinId) ->
			  del_from_collection_t(MixinId, [Id])
		  end, occi_resource:get_mixins(Res)),
    mnesia:delete({occi_resource, Id});

del_entity_t(#occi_link{id=Id, cid=Cid}=Link) ->
    del_from_collection_t(Cid, [Id]),
    lists:foreach(fun (MixinId) ->
			  del_from_collection_t(MixinId, [Id])
		  end, occi_link:get_mixins(Link)),
    mnesia:delete({occi_link, Id}).

del_from_collection_t(#occi_cid{}=Cid, Uris) ->
    lager:debug("Remove from collection ~p: ~p~n", [Cid, Uris]),
    case mnesia:wread({occi_collection, Cid}) of
	[#occi_collection{}=C] ->
	    mnesia:write(occi_collection:del_entities(C, Uris));
	[] ->
	    mnesia:abort({error, unknown_collection})
    end.

del_collection_t(#occi_collection{cid=#occi_cid{class=kind}=Cid}=Coll) ->
    Uris = occi_collection:get_entities(Coll),
    Colls = lists:foldl(fun (#uri{}=Uri, Acc) ->
				case mnesia:wread({occi_resource, Uri}) of
				    [#occi_resource{id=Id}=Res] ->
					mnesia:delete({occi_resource, Id}),
					del_node_t(occi_node:set_parent(occi_node:new(Uri, Res))),
					lists:foldl(fun (MixinId, Acc2) ->
							    dict:append(MixinId, Uri, Acc2)
						    end, Acc, occi_resource:get_mixins(Res));
				    [] ->
					case mnesia:wread({occi_link, Uri}) of
					    [#occi_link{id=Id}=Link] ->
						mnesia:delete({occi_link, Id}),
						del_node_t(occi_node:set_parent(occi_node:new(Uri, Link))),
						lists:foldl(fun (MixinId, Acc2) ->
								    dict:append(MixinId, Uri, Acc2)
							    end, Acc, occi_link:get_mixins(Link));
					    [] ->
						mnesia:abort({error, unknown_object})
					end
				end
			end, dict:new(), Uris),
    dict:map(fun (Key, Values) ->
		     del_from_collection_t(Key, Values)
	     end, dict:append_list(Cid, Uris, Colls)),
    ok;

del_collection_t(#occi_collection{cid=#occi_cid{class=mixin}=Cid}=Coll) ->
    Mixin = get_mixin_t(Cid),
    del_collection_t(Coll, Mixin).

del_collection_t(#occi_collection{cid=#occi_cid{class=mixin}=Cid}=Coll, #occi_mixin{}=Mixin) ->
    Entities = occi_collection:get_entities(Coll),
    lists:foreach(fun (Uri) ->
			  case mnesia:wread({occi_resource, Uri}) of
			      [#occi_resource{}=Res] ->
				  mnesia:write(occi_resource:del_mixin(Res, Mixin));
			      [] ->
				  mnesia:abort({error, unknown_object})
			  end
		  end, Entities),
    del_from_collection_t(Cid, Entities).

del_full_collection_t(#occi_collection{cid=#occi_cid{class=mixin}=Cid}=Coll, #occi_mixin{}=Mixin) ->
    Entities = occi_collection:get_entities(Coll),
    lists:foreach(fun (Uri) ->
			  case mnesia:wread({occi_resource, Uri}) of
			      [#occi_resource{}=Res] ->
				  mnesia:write(occi_resource:del_mixin(Res, Mixin));
			      [] ->
				  mnesia:abort({error, unknown_object})
			  end
		  end, Entities),
    mnesia:delete({occi_collection, Cid}).

del_mixin_t(#occi_mixin{id=Cid}=Mixin) ->
    case mnesia:wread({occi_collection, Cid}) of
	[#occi_collection{}=Coll] ->
	    del_full_collection_t(Coll, Mixin);
	[] ->
	    ok
    end,
    mnesia:delete({occi_mixin, Cid}).

get_mixin_t(#occi_cid{class=usermixin}=Cid) ->
    case mnesia:wread({occi_mixin, Cid}) of
	[#occi_mixin{}=M] ->
	    M;
	[] ->
	    case catch occi_category_mgr:get(Cid) of
		{error, Err} ->
		    mnesia:abort({error, Err});
		M ->
		    M
	    end
    end.
