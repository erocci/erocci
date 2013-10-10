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
	 set_type/2,
	 is_required/1, 
	 is_immutable/1, 
	 get_title/1,
	 get_default/1,
	 get_type/1]).

new(Id) ->
    #occi_attr_spec{id=Id}.

get_id(Spec) ->
    Spec#occi_attr_spec.id.

set_type(Spec, Type) ->
    Spec#occi_attr_spec{type=Type}.

is_required(Spec) ->
    is_defined(Spec, required).

is_immutable(Spec) ->
    is_defined(Spec, immutable).

get_title(Spec) ->
    get_value(Spec, title).

get_default(Spec) ->
    get_value(Spec, default).

get_type(#occi_attr_spec{type={Name, _Check}}) ->
    Name;
get_type(#occi_attr_spec{type=Name}) ->
    Name.

%%%
%%% Private functions
%%%
is_defined(#occi_attr_spec{properties=Properties}, Key) when is_list(Properties) ->
    lists:member(Key, Properties);
is_defined(_,_) ->
    false.

get_value(#occi_attr_spec{properties=Properties}, Key) when is_list(Properties) ->
    case lists:keyfind(Key, 1, Properties) of
	false -> undefined;	    
	{Key, Val} -> Val
    end;
get_value(_,_) ->
    undefined.
