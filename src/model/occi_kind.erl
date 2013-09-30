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
-export([init/2, 
	 get_attr/2,
	 get_obj/1,
	 get_collection/1]).

-record(data, {category      :: #occi_kind{},
	       backend       :: occi_store:backend_ref()}).

init(Backend, #occi_kind{}=Kind) ->
    #data{category=Kind, backend=Backend}.

get_attr(class, _Data) ->
    kind;
get_attr(scheme, #data{category=#occi_kind{id=#occi_cid{scheme=Scheme}}}) ->
    Scheme;
get_attr(term, #data{category=#occi_kind{id=#occi_cid{term=Term}}}) ->
    Term;
get_attr(title, #data{category=#occi_kind{title=Title}}) ->
    Title;
get_attr(attributes, #data{category=#occi_kind{attributes=Attributes}}) ->
    Attributes;
get_attr(parent, #data{category=#occi_kind{rel=Parent}}) ->
    Parent;
get_attr(actions, #data{category=#occi_kind{actions=Actions}}) ->
    Actions;
get_attr(location, #data{category=#occi_kind{location=Location}}) ->
    Location.

get_obj(#data{category=Obj}) ->
    Obj.

get_collection(#data{category=#occi_kind{id=Id}, backend=Ref}) ->
    occi_backend:find_all(Ref, Id).
