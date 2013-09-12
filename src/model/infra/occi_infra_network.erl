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
-module(occi_infra_network).

-occi_scheme('http://schemas.ogf.org/occi/infrastructure#').
-occi_term(network).
-occi_title(<<"Network resource">>).
-occi_class(kind).
-occi_entity_type(link).
-occi_relation({'http://schemas.ogf.org/occi/core#', link}).

%%%
%%% Attributes
%%% {Name :: atom(), Properties :: [occi_attr_property()], Checker :: mfa()}
%%%
-occi_attribute({'occi.network.vlan', {occi_types, is_range, [0, 4095]}}).
-occi_attribute({'occi.network.label', string}).
-occi_attribute({'occi.network.state', {occi_types, is_enum, [active, inactive, suspend]},
		[required, 
		 immutable,
		 {default, inactive},
		 {title, <<"State the network resource is in">>}]}).

%%%
%%% Actions
%%% {Name  :: atom(), Title :: binary(), Attributes :: [{occi_attr_key(), mfa()}]}
%%%
-occi_action({'http://schemas.ogf.org/occi/infrastructure/action#', 
	      up, 
	      <<"Put in active state">>, 
	      []}).
-occi_action({'http://schemas.ogf.org/occi/infrastructure/action#', 
	      down,
	      <<"Put in inactive state">>, 
	      []}).

%%%
%%% Implementation
%%%
-export([up/1, down/1]).

up(State) ->
    {ok, State}.

down(State) ->
    {ok, State}.
