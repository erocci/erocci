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

%% occi_category callbacks
-export([new/1,
	 new/2,
	 init/2,
	 set_parent/3]).

-export([impl_get_attr/2, impl_set_parent/3]).

-include("occi_category.hrl").

-record(data, {scheme       :: atom(),
	       term         :: atom(),
	       title,
	       attributes,
	       parent       :: occi_cid(),
	       actions,
	       location}).

new({Scheme, Term}) ->
    new([], {Scheme, Term}).

new(Mods, {Scheme, Term}) ->
    occi_category:new(lists:reverse([?MODULE|Mods]), {Scheme, Term}).

init(Scheme, Term) ->
    #data{scheme=Scheme, term=Term}.

set_parent(Ref, Scheme, Term) ->
    occi_object:call(Ref, impl_set_parent, [Scheme, Term]).

impl_get_attr(Data, class) ->
    {kind, Data};
impl_get_attr(#data{scheme=Scheme}=Data, scheme) ->
    {Scheme, Data};
impl_get_attr(#data{term=Term}=Data, term) ->
    {Term, Data};
impl_get_attr(#data{title=Title}=Data, title) ->
    {Title, Data};
impl_get_attr(#data{attributes=Attributes}=Data, attributes) ->
    {Attributes, Data};
impl_get_attr(#data{parent=Parent}=Data, parent) ->
    {Parent, Data};
impl_get_attr(#data{actions=Actions}=Data, actions) ->
    {Actions, Data};
impl_get_attr(#data{location=Location}=Data, location) ->
    {Location, Data}.

impl_set_parent(#data{}=Data, undefined, _Term) ->
    {{error, {einval, "Undefined scheme"}}, Data};
impl_set_parent(#data{}=Data, _Scheme, undefined) ->
    {{error, {einval, "Undefined term"}}, Data};
impl_set_parent(#data{}=Data, Scheme, Term) ->
    {ok, Data#data{parent=#occi_cid{scheme=Scheme, term=Term}}}.
