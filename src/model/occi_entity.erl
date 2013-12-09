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
%%% Created : 29 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_entity).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

%% from occi_object
-export([destroy/1,
	 save/1]).

%% OCCI entity methods
-export([new/2,
	 do/3,
	 to_plain/1]).
-export([impl_to_plain/1]).

%%%===================================================================
%%% API
%%%===================================================================
new(Mods, Args) ->
    occi_object:new(lists:reverse([?MODULE|Mods]), Args).

do(Ref, Action, Attributes) ->
    occi_object:call(Ref, impl_do, [Action, Attributes]).

to_plain(Ref) ->
    occi_object:call(Ref, to_plain, []).

%%
%% from occi_object
%%

%%
%% from occi_object
%%
destroy(Ref) -> 
    occi_object:destroy(Ref).

save(Ref) -> 
    occi_object:save(Ref).

%%%
%%% Fallback functions
%%%
impl_to_plain(Data) ->
    {{ok, occi_renderer_plain:render(Data)}, Data}.
