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
-export([render/1,
	 render_collection/1,
	 parse_resource_repr/1]).

%%%===================================================================
%%% API
%%%===================================================================
render_collection(Collection) ->
    Headers = lists:foldl(fun (Entity, Acc) ->
				  orddict:append(<<"x-occi-location">>, occi_resource:get_id(Entity), Acc)
			  end, orddict:new(), Collection),
    render_headers(Headers).

render(Category) when is_record(Category, occi_category); 
		      is_record(Category, occi_action) ->
    occi_renderer_text:render(Category, "\n\t");

render(Categories) ->
    lists:map(fun(Cat) -> [<<"Category: ">>, render(Cat), "\n"] end, Categories).

-spec parse_resource_repr(Bin :: binary()) -> [occi_entity()].
parse_resource_repr(Bin) ->
    occi_parser:parse_resource_repr(occi_scanner:scan(Bin)).

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
