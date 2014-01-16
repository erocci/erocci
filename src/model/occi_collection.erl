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
-module(occi_collection).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-export([new/1,
	 add_entity/2,
	 get_entities/1]).

new(#occi_cid{}=Cid) ->
    #occi_collection{cid=Cid, entities=[]}.

add_entity(#occi_collection{entities=E}=C, #occi_link{id=Id}) ->
    C#occi_collection{entities=[Id|E]};
add_entity(#occi_collection{entities=E}=C, #occi_resource{id=Id}) ->
    C#occi_collection{entities=[Id|E]};
add_entity(#occi_collection{entities=E}=C, Id) when is_list(Id) ->
    C#occi_collection{entities=[list_to_binary(Id)|E]}.

get_entities(#occi_collection{entities=E}) ->
    E.
