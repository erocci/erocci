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
%%% Created : 25 Jul 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_mixin).
-compile([{parse_transform, lager_transform}]).

-behaviour(occi_category).

-include("occi.hrl").

%% occi_category callbacks
-export([init/1, get/2]).

init(#occi_mixin{}=Mixin) ->
    Mixin.

get(class, Data) ->
    {mixin, Data};
get(scheme, #occi_mixin{id=#occi_cid{scheme=Scheme}}=Data) ->
    {Scheme, Data};
get(term, #occi_mixin{id=#occi_cid{term=Term}}=Data) ->
    {Term, Data};
get(title, #occi_mixin{title=Title}=Data) ->
    {Title, Data};
get(attributes, #occi_mixin{attributes=Attributes}=Data) ->
    {Attributes, Data};
get(depends, #occi_mixin{depends=Depends}=Data) ->
    {Depends, Data};
get(applies, #occi_mixin{applies=Applies}=Data) ->
    {Applies, Data};
get(actions, #occi_mixin{actions=Actions}=Data) ->
    {Actions, Data};
get(location, #occi_mixin{location=Location}=Data) ->
    {Location, Data}.
