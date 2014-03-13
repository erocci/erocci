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
%%% Created : 19 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_attribute).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-export([new/1,
	 get_id/1,
	 get_type/1,
	 get_type_id/1,
	 set_type/2,
	 is_required/1,
	 set_required/2,
	 is_immutable/1,
	 set_immutable/2,
	 get_default/1,
	 set_default/2,
	 set_value/2,
	 get_value/1,
	 set_title/2,
	 get_title/1,
	 check/1]).

-export([reset/1]).

-define(attr_default, [{immutable, false},
		       {required, false},
		       {default, undefined}]).

new(Id) when is_binary(Id) ->
    new(list_to_atom(binary_to_list(Id)));
new(Id) ->
    #occi_attr{id=Id, properties=dict:from_list(?attr_default)}.

get_id(A) ->
    A#occi_attr.id.

get_type_id(#occi_attr{type_id={_Ns, Id}}) ->
    Id.

get_type(#occi_attr{type_id=Id}) ->
    Id.

set_type(A, Id) ->
    A#occi_attr{type_id=Id, f=occi_type:get(Id)}.

is_required(A) ->
    dict:fetch(required, A#occi_attr.properties).

set_required(A, Req) ->
    A#occi_attr{properties=dict:store(required, Req, A#occi_attr.properties)}.

is_immutable(A) ->
    dict:fetch(immutable, A#occi_attr.properties).

set_immutable(#occi_attr{properties=Props}=A, Val) ->
    A#occi_attr{properties=dict:store(immutable, Val, Props)}.

get_default(#occi_attr{properties=Props}) ->
    dict:fetch(default, Props).

set_default(#occi_attr{properties=Props}=A, Value) ->
    A#occi_attr{properties=dict:store(default, Value, Props)}.

get_value(#occi_attr{value=Value}) ->
    Value.

set_value(#occi_attr{f=Fun}=A, Value) ->
    A#occi_attr{value=Fun(Value)}.

set_title(A, Title) ->
    A#occi_attr{title=Title}.

get_title(#occi_attr{title=Title}) ->
    Title.

-spec reset(occi_attr()) -> occi_attr().
reset(#occi_attr{}=A) ->
    A#occi_attr{value=get_default(A)}.

check(#occi_attr{value=undefined}=A) ->
    case is_required(A) of
	true -> error;
	false -> ok
    end;
check(#occi_attr{}=_A) ->
    ok.
