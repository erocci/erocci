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
%%% Created : 29 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%% implementation callbacks
-include("occi.hrl").

-export([destroy/1, 
	 save/1,
	 get_attr/2,
	 set_attr/3,
	 set_attrs/2]).

destroy(Ref) -> occi_object:destroy(Ref).

save(Ref) -> occi_object:save(Ref).

get_attr(Ref, Name) -> occi_object:get_attr(Ref, Name).

set_attr(Ref, Name, Value) -> occi_object:set_attr(Ref, Name, Value).

set_attrs(Ref, Attributes) -> occi_object:set_attrs(Ref, Attributes).
