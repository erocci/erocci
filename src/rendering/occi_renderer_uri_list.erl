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
-compile({parse_transform, lager_transform}).

-behaviour(occi_renderer).

-include("occi.hrl").

%% API
-export([render/1]).

%%%===================================================================
%%% API
%%%===================================================================
render(#occi_node{type=dir}=Node) ->
    occi_renderer:join(render_dir(Node), "\n");

render(#occi_node{type=occi_collection, data=Coll}) ->
    occi_renderer:join([ occi_uri:to_iolist(Id) || Id <- occi_collection:get_entities(Coll) ], <<"\n">>);

render(#occi_node{type=occi_query, data={Kinds, Mixins, Actions}}) ->
    occi_renderer:join(
      lists:reverse(
	lists:foldl(fun (#occi_kind{location=#uri{}=Uri}, Acc) ->
			    [occi_uri:to_iolist(Uri)|Acc];
			(#occi_mixin{location=#uri{}=Uri}, Acc) ->
			    [occi_uri:to_iolist(Uri)|Acc];
			(#occi_action{location=undefined}, Acc) ->
			    Acc;
			(#occi_action{location=#uri{}=Uri}, Acc) ->
			    [occi_uri:to_iolist(Uri)|Acc]
		    end, [], Kinds ++ Mixins ++ Actions)),
      <<"\n">>).    

%%%
%%% Prive
%%%
render_dir(#occi_node{id=Id, type=dir, data=Children}) ->
    L = gb_sets:fold(fun (#occi_node{}=Child, Acc) ->
			     [ render_dir(Child) | Acc ]
		     end, [], Children),
    occi_renderer:join([occi_uri:to_iolist(Id) | L], 
		       <<"\n">>);
render_dir(#occi_node{id=Id}) ->
    [ occi_uri:to_iolist(Id) ++ "\n" ].
