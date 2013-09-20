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
	 get/3,
	 find/3,
	 update/3,
	 delete/3]).

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

get(CatId, Id, #state{pb=Pid}=State) ->
    {ok, Obj} = riakc_pb_socket:get(Pid,
				    occi_tools:to_binary(CatId),
				    Id),
    Resource = occi_tools:from_json(riakc_obj:get_value(Obj)),
    {{ok, Resource}, State}.

find(_CatId, _Filter, State) ->
    % Not implemented yet
    {{ok, []}, State}.

update(CatId, Entity, #state{pb=Pid}=State) ->
    % Update looks the same as create as there is no difference
    % between riak key and entity id
    Obj = riakc_obj:new(occi_tools:to_binary(CatId),
			occi_tools:get_entity_id(Entity),
			occi_tools:to_json(Entity)),
    riakc_pb_socket:put(Pid, Obj),
    {ok, State}.

delete(_CatId,_Id, State) ->
    {ok, State}.

-spec valid_config(Opts::occi_backend:backend_opts()) -> occi_backend:backend_opts().
valid_config(Opts) ->
    Address = case lists:keyfind(ip, 1, Opts) of
		  false ->
		      {127,0,0,1};
		  {ip, Bin} ->
		      Str = binary_to_list(Bin),
		      case inet_parse:address(Str) of
			  {ok, _Ip} -> Str;
			  {error, einval} -> 
			      lager:error("Invalid address: ~p~n", [Str]),
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
