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
    case mnesia:transaction(save_t(Obj)) of
	{atomic, ok} ->
	    {ok, State};
	{aborted, Reason} ->
	    {{error, Reason}, State}
    end.

delete(#occi_resource{id=Id, cid=Cid}=Res, State) ->
    F = fun() ->
		case mnesia:wread({occi_collection, Cid}) of
		    [#occi_collection{}=C] ->
			mnesia:write(occi_collection:del_entity(C, Id));
		    [] ->
			mnesia:abort({error, unknown_collection})
		end,
		lists:foreach(fun (#occi_mixin{id=MixinId}) ->
				      case mnesia:wread({occi_collection, MixinId}) of
					  [#occi_collection{}=C1] ->
					      mnesia:write(occi_collection:del_entity(C1, Id));
					  [] ->
					      mnesia:abort({error, unknown_collection})
				      end
			      end, occi_resource:get_mixins(Res)),
		mnesia:delete_object(Res)
	end,
    case mnesia:transaction(F) of
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
    fun() ->
	    mnesia:write(Res),
	    KindId = occi_resource:get_cid(Res),
	    Uri = occi_resource:get_id(Res),
	    case mnesia:wread({occi_collection, KindId}) of
		[#occi_collection{}=C] ->
		    lager:debug("Update collection: ~p~n", [KindId]),
		    mnesia:write(occi_collection:add_entity(C, Uri));
		_ ->
		    % Create collection on the fly
		    lager:debug("Create collection: ~p~n", [KindId]),
		    mnesia:write(occi_collection:new(KindId, [Uri]))
	    end,
	    lists:foreach(fun (#occi_mixin{id=Id}) ->
				  case mnesia:wread({occi_collection, Id}) of
				      [#occi_collection{}=C1] ->
					  lager:debug("Update collection: ~p~n", [Id]),
					  mnesia:write(occi_collection:add_entity(C1, Uri));
				      _ ->
					  lager:debug("Create collection: ~p~n", [Id]),
					  mnesia:write(#occi_collection{cid=Id, entities=[Uri]})
				  end
			  end, occi_resource:get_mixins(Res))
    end;
save_t(#occi_mixin{}=Mixin) ->
    fun () ->
	    mnesia:write(Mixin)
    end;
save_t(#occi_collection{}=Coll) ->
    fun() ->
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
	    mnesia:write(Coll)
    end.
