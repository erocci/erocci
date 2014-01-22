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
	 terminate/1,
	 associate_mixin/3,
	 save/2,
	 delete/2,
	 find/2]).

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
    mnesia:wait_for_tables([occi_resource, occi_category], infinite),
    {ok, #state{}}.

terminate(#state{}) ->
    ok.

save(Obj, State) ->
    case mnesia:transaction(fun () -> save_t(Obj) end) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.

delete(#occi_resource{}=Res, State) ->
    case mnesia:transaction(fun () -> del_resource_t(Res) end) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end;

delete(#occi_collection{}=Coll, State) ->
    case mnesia:transaction(fun () -> del_collection_t(Coll) end) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.

associate_mixin(#occi_cid{}=Cid, Uris, State) ->
    case mnesia:transaction(fun () -> associate_mixin_t(Cid, Uris) end) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.    

find(#occi_mixin{}=Req, #state{}=State) ->
    Res = mnesia:dirty_match_object(Req),
    {{ok, Res}, State};
find(#occi_collection{}=Req, #state{}=State) ->
    Res = mnesia:dirty_match_object(Req),
    {{ok, Res}, State};
find(#occi_entity{id=Uri, cid=Cid, mixins=Mixins}, State) ->
    case find(#occi_resource{id=Uri, cid=Cid, mixins=Mixins, _='_'}, State) of
	{{ok, []}, State2} ->
	    find(#occi_link{id=Uri, cid=Cid, mixins=Mixins, _='_'}, State2);
	{{ok, Res}, State2} ->
	    {{ok, Res}, State2}
    end;
find(#occi_resource{}=Req, #state{}=State) ->
    Res = mnesia:dirty_match_object(Req),
    {{ok, Res}, State};
find(#occi_link{}=Req, #state{}=State) ->
    Res = mnesia:dirty_match_object(Req),
    {{ok, Res}, State};
find(_, #state{}=State) ->
    {{error, not_implemented}, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
save_t(#occi_resource{}=Res) ->
    mnesia:write(Res),
    KindId = occi_resource:get_cid(Res),
    Uri = occi_resource:get_id(Res),
    add_to_collection_t(KindId, [Uri]),
    lists:foreach(fun (#occi_mixin{id=MixinId}) ->
			  add_to_collection_t(MixinId, [Uri])
		  end, occi_resource:get_mixins(Res)),
    ok;

save_t(#occi_mixin{}=Mixin) ->
    mnesia:write(Mixin);

save_t(#occi_collection{}=Coll) ->
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

associate_mixin_t(#occi_cid{class=mixin}=Cid, Uris) ->
    Mixin = get_mixin_t(Cid),
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
    		  end, Uris),    
    add_to_collection_t(Cid, Uris).

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
    end.

del_resource_t(#occi_resource{id=Id, cid=Cid}=Res) ->
    del_from_collection_t(Cid, [Id]),
    lists:foreach(fun (MixinId) ->
			  del_from_collection_t(MixinId, [Id])
		  end, occi_resource:get_mixins(Res)),
    mnesia:delete_object(Res);

del_resource_t(#occi_link{id=Id, cid=Cid}=Link) ->
    del_from_collection_t(Cid, [Id]),
    lists:foreach(fun (MixinId) ->
			  del_from_collection_t(MixinId, [Id])
		  end, occi_resource:get_mixins(Link)),
    mnesia:delete_object(Link).

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
    Colls = lists:foldl(fun (Uri, Acc) ->
				case mnesia:wread({occi_resource, Uri}) of
				    [#occi_resource{}=Res] ->
					mnesia:delete_object(Res),
					lists:foldl(fun (MixinId, Acc2) ->
							    dict:append(MixinId, Uri, Acc2)
						    end, Acc, occi_resource:get_mixins(Res));
				    [] ->
					case mnesia:wread({occi_link, Uri}) of
					    [#occi_link{}=Link] ->
						mnesia:delete_object(Link),
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
    Entities = occi_collection:get_entities(Coll),
    Mixin = get_mixin_t(Cid),
    lists:foreach(fun (Uri) ->
			  case mnesia:wread({occi_resource, Uri}) of
			      [#occi_resource{}=Res] ->
				  mnesia:write(occi_resource:del_mixin(Res, Mixin));
			      [] ->
					  mnesia:abort({error, unknown_object})
			  end
		  end, Entities),
    del_from_collection_t(Cid, Entities).

get_mixin_t(#occi_cid{class=mixin}=Cid) ->
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
