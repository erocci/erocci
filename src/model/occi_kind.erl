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
-module(occi_kind).
-compile([{parse_transform, lager_transform}]).

-behaviour(occi_category).

-include("occi.hrl").

%% occi_category callbacks
-export([init/1, get/2]).

init(#occi_kind{}=Data) ->
    Data.

get(class, Data) ->
    {kind, Data};
get(scheme, #occi_kind{id=#occi_cid{scheme=Scheme}}=Data) ->
    {Scheme, Data};
get(term, #occi_kind{id=#occi_cid{term=Term}}=Data) ->
    {Term, Data};
get(title, #occi_kind{title=Title}=Data) ->
    {Title, Data};
get(attributes, #occi_kind{attributes=Attributes}=Data) ->
    {Attributes, Data};
get(parent, #occi_kind{rel=Parent}=Data) ->
    {Parent, Data};
get(actions, #occi_kind{actions=Actions}=Data) ->
    {Actions, Data};
get(location, #occi_kind{location=Location}=Data) ->
    {Location, Data}.
