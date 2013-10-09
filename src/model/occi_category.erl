%%%-------------------------------------------------------------------
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
%%% @doc OCCI Category type
%%%
%%% @end
%%% Created : 27 Sep 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_category).
-compile([{parse_transform, lager_transform}]).

%% API
-export([new/2]).

-include("occi_object.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec new(atom(), term()) -> {global, reference()} | {error, term()}.
new(Mods, Args) ->
    Ref = make_ref(),
    Mods2 = lists:reverse([occi_category|Mods]),
    ChildSpec = {Ref, 
		 {gen_server, start_link, [{global, Ref}, occi_object, {Mods2, Args}, []]},
		 permanent, brutal_kill, worker, [occi_object|Mods2]},
    case supervisor:start_child(occi_category_mgr, ChildSpec) of
	{ok, _Child} ->
	    {global, Ref};
	{ok, _Child, _Info} ->
	    {global, Ref};
	{error, Err} ->
	    {error, Err}
    end.
