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
-module(occi_infra_compute).

-occi_scheme('http://schemas.ogf.org/occi/infrastructure#').
-occi_term(compute).
-occi_title(<<"Compute resource">>).
-occi_class(kind).
-occi_entity_type(resource).
-occi_relation({'http://schemas.ogf.org/occi/core#', resource}).

%%%
%%% Attributes
%%% {Name :: atom(), Properties :: [occi_attr_property()], Checker :: mfa()}
%%%
-occi_attribute({'occi.compute.architecture', {occi_types, is_enum, [x86, x64]}}).
-occi_attribute({'occi.compute.cores', integer}).
-occi_attribute({'occi.compute.hostname', string}).
-occi_attribute({'occi.compute.speed', float}).
-occi_attribute({'occi.compute.memory', integer}).
-occi_attribute({'occi.compute.state', {occi_types, is_enum, [active, inactive, suspend]}, 
		 [required, 
		  immutable, 
		  {default, inactive},
		  {title, <<"State the compute resource is in">>}]
		}).

%%%
%%% Actions
%%% {Name  :: atom(), Title :: binary(), Attributes :: [{occi_attr_key(), mfa()}]}
%%%
-occi_action({'http://schemas.ogf.org/occi/infrastructure/action#', start, 
	      <<"Start Compute Resource">>, 
	      []}).
-occi_action({'http://schemas.ogf.org/occi/infrastructure/action#', 
	      stop,
	      <<"Stop Compute Resource">>, 
	      [
	       {method, {occi_types, is_enum, [graceful, acpioff, poweroff]}}
	      ]}).
-occi_action({'http://schemas.ogf.org/occi/infrastructure/action#', 
	      restart, 
	      <<"Restart Compute Resource">>, 
	      [
	       {method, {occi_types, is_enum, [graceful, warm, cold]}}
	      ]}).
-occi_action({'http://schemas.ogf.org/occi/infrastructure/action#', 
	      suspend, 
	      <<"Suspend Compute Resource">>, 
	      [
	       {method, {occi_types, is_enum, [hibernate, suspend]}}
	      ]}).

%%%
%%% Implementation
%%%
-export([start/1,
	 stop/2,
	 restart/2,
	 suspend/2]).

start(State) ->
    {ok, State}.

stop(State, _Method) ->
    {ok, State}.

restart(State, _Method) ->
    {ok, State}.

suspend(State, _Method) ->
    {ok, State}.
