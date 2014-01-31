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
render(#occi_node{type=occi_query, data={Kinds, Mixins, Actions}}) ->
    lists:map(fun(Cat) -> 
		      [<<"Category: ">>, occi_renderer_text:render(Cat, "\n\t"), "\n"] 
	      end, Kinds ++ Mixins ++ Actions);

render(#occi_node{type=occi_collection, data=Coll}) ->
    Headers = lists:foldl(fun (EntityId, Acc) ->
				  orddict:append(<<"x-occi-location">>, [occi_uri:to_iolist(EntityId)], Acc)
			  end, orddict:new(), occi_collection:get_entities(Coll)),
    render_headers(Headers).

%%
%% Private
%%
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

%%%
%%% Tests
%%%
-ifdef(TEST).

%% render1_test() ->
%%     T = occi_type:get_category(<<"http://localhost">>, <<"compute">>, occi_infra_compute),
%%     Expect = <<"compute; \n\tscheme=\"http://schemas.ogf.org/occi/infrastructure#\"; \n\tclass=\"kind\"; \n\ttitle=\"Compute resource\"; \n\trel=\"http://schemas.ogf.org/occi/core#resource\"; \n\tattributes=\"occi.compute.state{required,immutable} occi.compute.memory occi.compute.speed occi.compute.hostname occi.compute.cores occi.compute.architecture\"; \n\tactions=\"http://schemas.ogf.org/occi/infrastructure/compute/action#suspend http://schemas.ogf.org/occi/infrastructure/compute/action#restart http://schemas.ogf.org/occi/infrastructure/compute/action#stop http://schemas.ogf.org/occi/infrastructure/compute/action#start\"; \n\tlocation=\"http://localhost/compute/\"">>,
%%     ?assert(erlang:iolist_to_binary(occi_renderer_plain:render(T)) =:= Expect).

-endif.
