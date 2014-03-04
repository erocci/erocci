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
-module(occi_renderer_plain).
-compile({parse_transform, lager_transform}).

-behaviour(occi_renderer).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("occi.hrl").

%% API
-export([render/1]).

%%%===================================================================
%%% API
%%%===================================================================
render(#occi_node{type=dir}=Node) ->
    Headers = render_dir(Node, orddict:new()),
    render_headers(Headers);

render(#occi_node{type=occi_query, data={Kinds, Mixins, Actions}}) ->
    lists:map(fun(Cat) -> 
		      [<<"category: ">>, occi_renderer_text:render(Cat, "\n\t"), "\n"] 
	      end, Kinds ++ Mixins ++ Actions);

render(#occi_node{type=occi_collection, data=Coll}) ->
    Headers = lists:foldl(fun (EntityId, Acc) ->
				  orddict:append(<<"x-occi-location">>, [occi_uri:to_iolist(EntityId)], Acc)
			  end, orddict:new(), occi_collection:get_entities(Coll)),
    render_headers(Headers).

%%
%% Private
%%
render_dir(#occi_node{type=dir, data=Children}, Acc) ->
    gb_sets:fold(fun (#occi_node{type=dir}=Child, Acc2) ->
			 render_dir(Child, Acc2);
		     (#uri{}=ChildId, Acc2) ->
			 orddict:append(<<"x-occi-location">>, occi_uri:to_iolist(ChildId), Acc2)
		 end, Acc, Children).

render_headers(Headers) ->
    occi_renderer:join(
      lists:map(fun (Name) -> 
			render_header(Name, orddict:fetch(Name, Headers))
		end, orddict:fetch_keys(Headers)),
      "\n").

render_header(Name, Values) ->
    occi_renderer:join(
      lists:map(fun (Value) ->
			occi_renderer:join([Name, Value], ": ")
		end, Values),
      "\n").
