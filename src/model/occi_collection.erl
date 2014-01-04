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

-export([new/0,
	 get_resources/1,
	 set_resources/2,
	 add_resource/2,
	 get_links/1,
	 set_links/2,
	 add_link/2]).

new() ->
    #occi_collection{resources=[],
		     links=[]}.

get_resources(#occi_collection{resources=R}) ->
    R.

set_resources(#occi_collection{}=C, Resources) ->
    C#occi_collection{resources=Resources}.

add_resource(#occi_collection{resources=R}=C, Res) ->
    C#occi_collection{resources=[Res|R]}.

get_links(#occi_collection{links=L}) ->
    L.

set_links(#occi_collection{}=C, Links) ->
    C#occi_collection{links=Links}.

add_link(#occi_collection{links=L}=C, Link) ->
    C#occi_collection{links=[Link|L]}.
