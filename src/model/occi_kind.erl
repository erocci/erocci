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

-include("occi.hrl").

%% from occi_object
-export([destroy/1,
	 save/1]).

%% from occi_category
-export([get_id/1,
	 get_class/1,
	 get_scheme/1,
	 get_term/1,
	 get_title/1,
	 set_title/2,
	 add_attribute/2,
	 get_attributes/1,
	 get_attr_list/1,
	 set_types_check/2]).

-export([new/1,
	 init/2,
	 get_parent/1,
	 set_parent/3,
	 get_actions/1,
	 add_action/2]).

%% specific implementations
-export([impl_get_class/1,
	 impl_get_parent/1,
	 impl_set_parent/3]).

-record(data, {super            :: term(),
	       parent           :: occi_cid(),
	       actions    = []}).

%%
%% from occi_object
%%
destroy(O) -> 
    occi_category:destroy(O).

save(O) -> 
    occi_category:save(O).

%%
%% from occi_category
%%
get_id(O) -> 
    occi_category:get_id(O).

get_class(O) -> 
    occi_category:get_class(O).

get_scheme(O) -> 
    occi_category:get_scheme(O).

get_term(O) -> 
    occi_category:get_term(O).

get_title(O) -> 
    occi_category:get_title(O).

set_title(O, Title) -> 
    occi_category:set_title(O, Title).

add_attribute(O, A) -> 
    occi_category:add_attribute(O, A).

get_attributes(O) ->
    occi_category:get_attributes(O).

get_attr_list(O) ->
    occi_category:get_attr_list(O).

set_types_check(O, Types) -> 
    occi_category:set_types_check(O, Types).

get_actions(O) ->
    occi_category:get_actions(O).

add_action(O, Action) ->
    occi_category:add_action(O, Action).

%%
%% specific methods
%%
new({Scheme, Term}) ->
    Ref = occi_category:new([?MODULE], {Scheme, Term}),
    #occi_category{id=#occi_cid{scheme=Scheme, term=Term, class=kind}, ref=Ref}.

init(Scheme, Term) ->
    Cat = occi_category:init(Scheme, Term),
    #data{super=Cat}.

get_parent(O) ->
    occi_object:call(O, impl_get_parent, []).

set_parent(O, Scheme, Term) ->
    occi_object:call(O, impl_set_parent, [Scheme, Term]).

%%
%% implementations
%%
impl_get_class(Data) ->
    {{ok, kind}, Data}.

impl_get_parent(#data{parent=Parent}=Data) ->
    {{ok, Parent}, Data}.

impl_set_parent(#data{}=Data, undefined, _Term) ->
    {{error, {einval, "Undefined scheme"}}, Data};
impl_set_parent(#data{}=Data, _Scheme, undefined) ->
    {{error, {einval, "Undefined term"}}, Data};
impl_set_parent(#data{}=Data, Scheme, Term) ->
    {ok, Data#data{parent=#occi_cid{scheme=Scheme, term=Term}}}.
