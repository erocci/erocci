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
%%% Created : 12 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_infra_ipnetworking).

-occi_scheme('http://schemas.ogf.org/occi/infrastructure/network#').
-occi_term(ipnetwork).
-occi_title(<<"IP Networking Mixin">>).
-occi_class(mixin).

%%%
%%% Attributes
%%% {Name :: atom(), Properties :: [occi_attr_property()], Checker :: mfa()}
%%%
-occi_attribute({'occi.network.address', {cidr, {occi_types, is_cidr, []}}}).
-occi_attribute({'occi.network.gateway', {ip, {occi_types, is_ipaddress, []}}}).
-occi_attribute({'occi.network.allocation', 
		 {token, {occi_types, is_enum, [[dynamic, static]]}}}).

%%%
%%% Implementation
%%%
-export([up/1, down/1]).

up(State) ->
    {ok, State}.

down(State) ->
    {ok, State}.
