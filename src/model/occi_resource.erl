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
%%% Created : 30 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_resource).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-export([new/1,
	 new/2,
	 init/2]).

%% from occi_object
-export([destroy/1,
	 save/1]).

%% from occi_entity
-export([do/3]).

%% specific implementations
-export([impl_get_attr/2]).

-record(data, {category                 :: reference(),
	       mixins     = []          :: [reference()],
	       attributes = dict:new()  :: term()}).

%%%
%%% API
%%%
-spec new({Category :: reference(), Mixins :: [reference()]}) -> pid().
new({Category, Mixins}) ->
    new([], {Category, Mixins}).

new(Mods, [Category, Mixins]) ->
    occi_entity:new(lists:reverse([?MODULE|Mods]), {Category, Mixins}).

init(CategoryRef, MixinRefs) ->
    Attributes = occi_category:get_attr(CategoryRef, attributes),
    Attributes2 = lists:foldl(fun(Ref, Acc) ->
				      [occi_category:get_attr_specs(Ref, attributes)|Acc]
			      end, Attributes, MixinRefs),
    #data{category=CategoryRef, mixins=MixinRefs, attributes=dict:from_list(Attributes2)}.

%%
%% from occi_object
%%
destroy(Ref) -> 
    occi_category:destroy(Ref).

save(Ref) -> 
    occi_category:save(Ref).

%%
%% from occi_entity
%%
do(Ref, Action, Attributes) -> 
    occi_entity:do(Ref, Action, Attributes).

impl_get_attr(#data{}=Data, Name) ->
    case dict:find(Name, Data#data.attributes) of
	{ok, Value} ->
	    {{ok, Value}, Data};
	error ->
	    {{error, einval}, Data}
    end.
