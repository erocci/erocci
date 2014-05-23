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
	 load/2,
	 action/2]).

-record(state, {}).

%%%===================================================================
%%% occi_backend callbacks
%%%===================================================================
init(#occi_backend{ref=Ref, opts=Opts}) ->
    init_db(),
    case proplists:get_value(schemas, Opts) of
	undefined -> ok;
	Schemas ->
	    case occi_category_mgr:load_schemas(Ref, Schemas) of
		ok -> ok;
		{error, Err} -> throw({error, Err})
	    end
    end,
    {ok, #state{}}.

init_db() ->
    case mnesia:system_info(extra_db_nodes) of
	[] ->
	    mnesia:create_schema([node()]);
	_ ->
	    ok
    end,
    application:start(mnesia, permanent),
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
    mnesia:wait_for_tables([occi_collection, occi_resource, occi_link, occi_mixin, occi_node],
			   infinite).

terminate(#state{}) ->
    ok.

save(Obj, State) when is_record(Obj, occi_node);
		      is_record(Obj, occi_mixin) ->
    lager:info("[~p] save(~p)~n", [?MODULE, Obj]),
    case mnesia:transaction(fun () -> save_t(Obj) end) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.

delete(Obj, State) when is_record(Obj, occi_node);
			is_record(Obj, occi_mixin) ->
    lager:info("[~p] delete(~p)~n", [?MODULE, Obj]),
    case mnesia:transaction(fun () -> del_node_t(Obj) end) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.

update(#occi_node{}=Node, State) ->
    lager:info("[~p] update(~p)~n", [?MODULE, Node]),
    case mnesia:transaction(fun () -> update_t(Node) end) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.    

find(Obj, State) when is_record(Obj, occi_node);
		      is_record(Obj, occi_mixin) ->
    lager:info("[~p] find(~p)~n", [?MODULE, Obj]),
    case mnesia:transaction(fun () ->
				    find_node_t(Obj)
			    end) of
	{atomic, Res} -> 
	    {{ok, Res}, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.

load(#occi_node{}=Req, State) ->
    lager:info("[~p] load(~p)~n", [?MODULE, Req]),
    case mnesia:transaction(fun () ->
				    load_node_t(Req)
			    end) of
	{atomic, Res} -> 
	    {{ok, Res}, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.

action({#uri{}=Id, #occi_action{}=A}, State) ->
    lager:info("[~p] action(~p, ~p)~n", [?MODULE, Id, A]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
find_node_t(#occi_node{type=occi_query}) ->
    mnesia:match_object(#occi_mixin{_='_'});

find_node_t(#occi_mixin{}=Mixin) ->
    mnesia:match_object(Mixin);

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
    load_dir_t(occi_node:set_parent(Node)).

load_dir_t(#occi_node{data=Children}=Node) ->
    Children2 = gb_sets:fold(fun (ChildId, Acc) ->
				     case mnesia:wread({occi_node, ChildId}) of
					 [] ->
					     mnesia:abort({unknown_node, ChildId});
					 [#occi_node{type=dir}=Child] ->
					     gb_sets:add(load_dir_t(Child), Acc);
					 [#occi_node{id=Child}] ->
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

save_t(#occi_mixin{id=#occi_cid{class=usermixin}=Cid, location=Uri}=Mixin) ->
    mnesia:write(Mixin),
    save_node_t(occi_node:new(Uri, Cid));

save_t(#occi_node{type=occi_collection, data=Coll}) ->
    save_collection_t(Coll).

save_collection_t(#occi_collection{}=Coll) ->
    lists:foreach(fun (#uri{}=Id) ->
			  case check_entity_t(Id) of
			      true -> ok;
			      false -> mnesia:abort({unknown_entity, Id})
			  end
		  end, occi_collection:get_entities(Coll)),
    mnesia:write(Coll).

save_entity_t(#occi_resource{}=Res) ->
    mnesia:write(Res),
    KindId = occi_resource:get_cid(Res),
    Uri = occi_resource:get_id(Res),
    add_to_collection_t(KindId, [Uri]),
    sets:fold(fun (#occi_cid{}=MixinId, _) ->
		      add_to_collection_t(MixinId, [Uri])
	      end, ok, occi_resource:get_mixins(Res)),
    ok;

save_entity_t(#occi_link{}=Link) ->
    add_link_t(Link, occi_link:get_source(Link)),
    add_link_t(Link, occi_link:get_target(Link)),
    mnesia:write(Link),
    KindId = occi_link:get_cid(Link),
    Uri = occi_link:get_id(Link),
    add_to_collection_t(KindId, [Uri]),
    sets:fold(fun (#occi_cid{}=MixinId, _) ->
		      add_to_collection_t(MixinId, [Uri])
	      end, ok, occi_link:get_mixins(Link)),
    ok.

add_link_t(Link, ResId) ->
    Res2 = case occi_uri:is_rel(ResId) of
	       true ->
		   case mnesia:wread({occi_resource, ResId}) of
		       [] ->
			   mnesia:abort({unknown_resource, ResId});
		       [Res] ->
			   occi_resource:add_link(Res, occi_link:get_id(Link))
		   end;
	       false ->
		   case occi_store:find(#occi_node{id=ResId, _='_'}) of
		       {ok, []} ->
			   mnesia:abort({unknown_resource, ResId});
		       {ok, [#occi_resource{}=Res]} ->
			  occi_resource:add_link(Res, occi_link:get_id(Link))
		   end
	   end,
    mnesia:write(Res2).

save_node_t(#occi_node{id=Id}=Node) ->
    case mnesia:wread({occi_node, Id}) of
	[_] -> 	
	    mnesia:write(Node#occi_node{data=undefined});
	[] -> 
	    mnesia:write(Node#occi_node{data=undefined}),
	    add_to_dir_t(occi_uri:get_parent(Id), Id)
    end.

add_to_dir_t(none, _) ->
    ok;
add_to_dir_t(#uri{path=Path}=Parent, Child) ->
    lager:debug("add_to_dir_t(~p, ~p)~n", [Parent, Child]),
    case mnesia:wread({occi_node, Parent}) of
	[] ->
	    Node = occi_node:new(Parent, dir),
	    mnesia:write(occi_node:add_child(Node, Child));
	[#occi_node{type=dir}=Node] ->
	    mnesia:write(occi_node:add_child(Node, Child));
	[#occi_node{}] ->
	    mnesia:abort({not_a_dir, Parent})
    end,
    if Path == [] -> ok;
       true -> add_to_dir_t(occi_uri:get_parent(Parent), Parent)
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

del_node_t(#occi_mixin{id=Cid}=Mixin) ->
    del_mixin_t(Mixin),
    mnesia:delete({occi_mixin, Cid});

del_node_t(#occi_node{type=occi_collection, data=Coll}) ->
    del_collection_t(Coll);

del_node_t(#occi_node{type=occi_resource, data=undefined}=Node) ->
    del_node_t(load_node_t(Node));

del_node_t(#occi_node{id=Id, type=occi_resource, data=Res}=Node) ->
    del_entity_t(Res),
    del_from_parent_t(Node),
    mnesia:delete({occi_node, Id});

del_node_t(#occi_node{type=occi_link, data=undefined}=Node) ->
    del_node_t(load_node_t(Node));

del_node_t(#occi_node{id=Id, type=occi_link, data=Res}=Node) ->
    del_entity_t(Res),
    del_from_parent_t(Node),
    mnesia:delete({occi_node, Id});

del_node_t(#occi_node{id=Id, type=dir}=Node) ->
    Node2 = load_node_t(Node),
    del_dir_t(Node2),
    del_from_parent_t(Node2),
    mnesia:delete({occi_node, Id}).

del_dir_t(#occi_node{id=Id, type=dir, data=Children}) ->
    gb_sets:fold(fun (#occi_node{type=dir}=Child, _) ->
			 del_dir_t(Child);
		     (#occi_node{}=Child, _) ->
			 del_node_t(Child)
		 end, ok, Children),
    mnesia:delete({occi_node, Id}).

del_from_parent_t(#occi_node{id=Id}) ->
    ParentId = occi_uri:get_parent(Id),
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
    sets:fold(fun (MixinId, _) ->
		      del_from_collection_t(MixinId, [Id])
		  end, ok, occi_resource:get_mixins(Res)),
    mnesia:delete({occi_resource, Id});

del_entity_t(#occi_link{id=Id, cid=Cid}=Link) ->
    del_from_collection_t(Cid, [Id]),
    sets:fold(fun (MixinId, _) ->
		      del_from_collection_t(MixinId, [Id])
	      end, ok, occi_link:get_mixins(Link)),
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
					sets:fold(fun (MixinId, Acc2) ->
							  dict:append(MixinId, Uri, Acc2)
						  end, Acc, occi_resource:get_mixins(Res));
				    [] ->
					case mnesia:wread({occi_link, Uri}) of
					    [#occi_link{id=Id}=Link] ->
						mnesia:delete({occi_link, Id}),
						del_node_t(occi_node:set_parent(occi_node:new(Uri, Link))),
						sets:fold(fun (MixinId, Acc2) ->
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
	    case occi_store:find(Cid) of
		{ok, [#occi_mixin{}=Mixin]} -> Mixin;
		_ -> mnesia:abort({error, {invalid_cid, Cid}})
	    end
    end;
get_mixin_t(#occi_cid{}=Cid) ->
    case occi_store:find(Cid) of
	{ok, [#occi_mixin{}=Mixin]} -> Mixin;
	_ -> mnesia:abort({error, {invalid_cid, Cid}})
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
