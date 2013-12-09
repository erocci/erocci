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
-module(occi_link).
-compile([{parse_transform, lager_transform}]).

-export([new/1,
	 new/2, 
	 init/4]).

-include("occi.hrl").

%% from occi_object
-export([destroy/1,
	 save/1]).

%% from occi_entity
-export([do/3]).

-record(data, {category   :: reference(),
	       mixins     :: [reference()],
	       data       :: occi_link()}).

%%%
%%% API
%%%
-spec new({Category :: reference(), Mixins :: [reference()], Source :: uri(), Target :: uri()}) -> pid().
new({Category, Mixins, Source, Target}) ->
    new([], {Category, Mixins, Source, Target}).

new(Mods, {Category, Mixins, Src, Target}) ->
    occi_entity:new(lists:reverse([?MODULE|Mods]), {Category, Mixins, Src, Target}).

init(CategoryRef, MixinRefs, Src, Target) ->
    #data{category=CategoryRef, 
	  mixins=MixinRefs,
	 data=#occi_link{source=Src, target=Target}}.

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
