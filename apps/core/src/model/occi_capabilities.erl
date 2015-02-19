%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
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
%%% Created : 1 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_capabilities).

-include("occi.hrl").

-export([new/0,
	 new/3,
	 set/4,
	 add_mixin/2,
	 merge/2]).

-spec new() -> occi_node().
new() ->
    #occi_node{id=#uri{path="/-/"}, type=capabilities, objid=capabilities, owner=nobody, data={[],[],[]}}.

-spec new([occi_kind()], [occi_mixin()], [occi_action()]) -> occi_node().
new(Kinds, Mixins, Actions) when is_list(Kinds),
				 is_list(Mixins),
				 is_list(Actions) ->
    #occi_node{id=#uri{path="/-/"}, objid=capabilities, type=capabilities, owner=nobody, 
	       data={Kinds, Mixins, Actions}}.

-spec set(occi_node(), [occi_kind()], [occi_mixin()], [occi_action()]) -> occi_node().
set(#occi_node{type=capabilities}=N, Kinds, Mixins, Actions) when is_list(Kinds),
								  is_list(Mixins),
								  is_list(Actions) ->
    N#occi_node{data={Kinds, Mixins, Actions}}.
    
-spec add_mixin(occi_node(), occi_mixin()) -> occi_node().
add_mixin(#occi_node{type=capabilities, data={K, M, A}}=N, #occi_mixin{}=Mixin) ->
    N#occi_node{data={K, [Mixin | M], A}};
add_mixin(#occi_node{type=capabilities}=N, #occi_mixin{}=Mixin) ->
    N#occi_node{data={[], [Mixin], []}}.

-spec merge(occi_node(), occi_node()) -> occi_node().
merge(#occi_node{type=capabilities, data={K1, M1, A1}}=C1, 
      #occi_node{type=capabilities, data={K2, M2, A2}}) ->
    C1#occi_node{data={K1++K2, M1++M2, A1++A2}};
merge(#occi_node{type=capabilities}=C1, 
      #occi_node{type=capabilities, data={K2, M2, A2}}) ->
    C1#occi_node{data={K2, M2, A2}}.
