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
	 set_types_check/2,
	 get_actions/1,
	 add_action/2]).

-export([new/1, 
	 init/2,
	 get_applies/1,
	 add_applies/3,
	 get_depends/1,
	 add_depends/3]).

-export([impl_get_class/1,
	 impl_get_applies/1,
	 impl_add_applies/3,
	 impl_get_depends/1,
	 impl_add_depends/3]).

-record(data, {super               :: term(),
	       applies    = []     :: [occi_cid()],
	       depends    = []     :: [occi_cid()],
	       actions}).

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

set_types_check(O, Types) -> 
    occi_category:set_types_check(O, Types).

get_actions(O) ->
    occi_category:get_actions(O).

add_action(O, Action) ->
    occi_category:add_action(O, Action).

%%
%% Specific methods
%%
new({Scheme, Term}) ->
    Ref = occi_category:new([?MODULE], {Scheme, Term}),
    #occi_category{id=#occi_cid{scheme=Scheme, term=Term, class=mixin}, ref=Ref}.

init(Scheme, Term) ->
    Cat = occi_category:init(Scheme, Term),
    #data{super=Cat}.

get_applies(O) ->
    occi_object:call(O, impl_get_applies, []).

add_applies(O, Scheme, Term) ->
    occi_object:call(O, impl_add_applies, [Scheme, Term]).

get_depends(O) ->
    occi_object:call(O, impl_get_depends, []).

add_depends(O, Scheme, Term) ->
    occi_object:call(O, impl_add_depends, [Scheme, Term]).

%%
%% implementations
%%
impl_get_class(Data) ->
    {{ok, mixin}, Data}.

impl_get_applies(#data{applies=Applies}=Data) ->
    {{ok, Applies}, Data}.

impl_add_applies(#data{}=Data, undefined, _Term) ->
    {{error, {einval, "Undefined scheme"}}, Data};
impl_add_applies(#data{}=Data, _Scheme, undefined) ->
    {{error, {einval, "Undefined term"}}, Data};
impl_add_applies(#data{applies=Applies}=Data, Scheme, Term) ->
    {ok, Data#data{applies=[#occi_cid{scheme=Scheme, term=Term}|Applies]}}.

impl_get_depends(#data{depends=Depends}=Data) ->
    {{ok, Depends}, Data}.

impl_add_depends(#data{}=Data, undefined, _Term) ->
    {{error, {einval, "Undefined scheme"}}, Data};
impl_add_depends(#data{}=Data, _Scheme, undefined) ->
    {{error, {einval, "Undefined term"}}, Data};
impl_add_depends(#data{depends=Depends}=Data, Scheme, Term) ->
    {ok, Data#data{depends=[#occi_cid{scheme=Scheme, term=Term}|Depends]}}.
