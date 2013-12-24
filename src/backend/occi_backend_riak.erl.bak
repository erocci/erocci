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
-module(occi_backend_riak).
-compile({parse_transform, lager_transform}).

-behaviour(occi_backend).

-include("occi.hrl").

%% occi_backend callbacks
-export([init/1,
	 terminate/1,
	 save/2,
	 find_all/2,
	 load_entities/3,
	 load_entity/3]).

-record(state, {pb :: pid()}).

%%%===================================================================
%%% occi_backend callbacks
%%%===================================================================
init(Opts) ->
    Opts2 = valid_config(Opts),
    {ip, Node} = lists:keyfind(ip, 1, Opts2),
    {port, Port} = lists:keyfind(port, 1, Opts2),
    case riakc_pb_socket:start_link(Node, Port) of
	{ok, Pid} -> 
	    {ok, #state{pb=Pid}};
	{error, Error} ->
	    lager:error("Error starting riak client: ~p~n", [Error]),
	    {error, Error}
    end.

terminate(#state{pb=Pid}) ->
    riakc_pb_socket:stop(Pid).

save(Obj, #state{pb=Pid}=State) when is_record(Obj, occi_resource);
				     is_record(Obj, occi_link) ->
    RObj = riakc_obj:new(occi_renderer_json:render(occi_entity:get_cid(Obj)),
			 occi_entity:get_id(Obj),
			 occi_renderer_json:render(Obj)),
    Ret = case riakc_pb_socket:put(Pid, RObj) of
	      ok -> {ok, Obj};
	      {ok, Key} -> {ok, occi_entity:set_id(Obj, Key)};
	      {error, Err} -> {error, Err}
	  end,
    {Ret, State}.

find_all(#occi_cid{}=Id, #state{pb=Pid}=State) ->
    Entities = case riakc_pb_socket:stream_list_keys(Pid, occi_renderer_json:render(Id)) of
		   {ok, ReqId} ->
		       wait_for_keys(ReqId, [], Id, State);
		   Error ->
		       throw({error, Error})
	       end,
    {Entities, State}.

-spec load_entities(occi_cid() | binary(), [uri()], #state{}) -> [occi_entity()].
load_entities(CatId, Ids, #state{}=State) ->
    [ load_entity(CatId, Id, State) || Id <- Ids ].

-spec load_entity(occi_cid() | binary(), uri(), #state{}) -> occi_entity().
load_entity(#occi_cid{}=CatId, Id, State) ->
    load_entity(occi_renderer_json:render(CatId), Id, State);
load_entity(CatId, Id, #state{pb=Pid}=State) ->
    case riakc_pb_socket:get(Pid, CatId, Id) of
	{ok, Obj} ->
	    lager:info("OBJECT VALUE: ~p~n", [riakc_obj:get_value(Obj)]),
	    {occi_renderer_json:parse(riakc_obj:get_value(Obj)), State};
	{error, Err} ->
	    throw({occi_backend_error, Err})
    end.

-spec valid_config(Opts::occi_backend:backend_opts()) -> occi_backend:backend_opts().
valid_config(Opts) ->
    Address = case lists:keyfind(ip, 1, Opts) of
		  false ->
		      {127,0,0,1};
		  {ip, Val} ->
		      IpStr = to_list(Val),
		      case inet_parse:address(IpStr) of
			  {ok, _Ip} -> Val;
			  {error, einval} -> 
			      lager:error("Invalid address: ~p~n", [Val]),
			      throw(einval)
		      end
	      end,
    Port = case lists:keyfind(port, 1, Opts) of
	       false ->
		   8087;
	       {port, I} -> I
	   end,
    [{ip, Address}, {port, Port}].

%%%===================================================================
%%% Internal functions
%%%===================================================================
to_list(S) when is_list(S) ->
    S;
to_list(S) when is_binary(S) ->
    binary_to_list(S).

wait_for_keys(ReqId, Acc, CatId, State) ->
    receive
	{ReqId, done} ->
	    lists:flatten(Acc);
	{ReqId, {keys, Res}} ->
	    wait_for_keys(ReqId, [load_entities(CatId, Res, State)|Acc], CatId, State);
	{ReqId, {error, Reason}} -> 
	    {error, Reason}
    end.
