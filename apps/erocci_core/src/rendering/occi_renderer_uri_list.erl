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
-module(occi_renderer_uri_list).

-behaviour(occi_renderer).

-include("occi.hrl").

%% API
-export([render/2]).

%%%===================================================================
%%% API
%%%===================================================================
render(#occi_node{type=occi_collection, data=Coll}, Env) ->
    Data = lists:map(fun (#uri{}=Id) ->
			     [ occi_uri:to_iolist(Id, Env), "\n"];
			 (#occi_node{id=Id}) ->
			     [ occi_uri:to_iolist(Id, Env), "\n" ]
		     end, occi_collection:get_entities(Coll)),
    {Data, Env};

render(#occi_node{type=capabilities, data={Kinds, Mixins, Actions}}, Env) ->
    Data = occi_renderer:join(
	     lists:reverse(
	       lists:foldl(fun (#occi_kind{location=#uri{}=Uri}, Acc) ->
				   [occi_uri:to_iolist(Uri, Env)|Acc];
			       (#occi_mixin{location=#uri{}=Uri}, Acc) ->
				   [occi_uri:to_iolist(Uri, Env)|Acc];
			       (#occi_action{location=undefined}, Acc) ->
				   Acc;
			       (#occi_action{location=#uri{}=Uri}, Acc) ->
				   [occi_uri:to_iolist(Uri, Env)|Acc]
			   end, [], Kinds ++ Mixins ++ Actions)),
	     <<"\n">>),
    {Data, Env}.

%%%
%%% Private
%%%
