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
%%% Created :  1 Jul 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_backend_dummy).
-compile({parse_transform, lager_transform}).

-behaviour(occi_backend).

-include("occi.hrl").

%% occi_backend callbacks
-export([init/1,
	 terminate/1]).
-export([update/2,
	 save/2,
	 delete/2,
	 find/2,
	 load/2]).

-record(state, {id             :: atom(),
		wait           :: integer()}).

%%%===================================================================
%%% occi_backend callbacks
%%%===================================================================
init(Props) ->
    {ok, #state{id=proplists:get_value(ref, Props),
		wait=proplists:get_value(wait, Props, 0)}}.

terminate(#state{}) ->
    ok.

save(Obj, #state{id=Id, wait=Wait}=State) when is_record(Obj, occi_node);
		      is_record(Obj, occi_mixin) ->
    lager:info("[~p] save(~p)~n", [Id, lager:pr(Obj, ?MODULE)]),
    timer:sleep(Wait),
    {ok, State}.

delete(Obj, #state{id=Id, wait=Wait}=State) when is_record(Obj, occi_node);
			is_record(Obj, occi_mixin) ->
    lager:info("[~p] delete(~p)~n", [Id, lager:pr(Obj, ?MODULE)]),
    timer:sleep(Wait),
    {ok, State}.

update(#occi_node{}=Node, #state{id=Id, wait=Wait}=State) ->
    lager:info("[~p] update(~p)~n", [Id, lager:pr(Node, ?MODULE)]),
    timer:sleep(Wait),
    {ok, State}.

find(Obj, #state{id=Id, wait=Wait}=State) when is_record(Obj, occi_node);
				    is_record(Obj, occi_mixin) ->
    lager:info("[~p] find(~p)~n", [Id, lager:pr(Obj, ?MODULE)]),
    timer:sleep(Wait),
    {{ok, []}, State}.

load(#occi_node{}=Req, #state{id=Id, wait=Wait}=State) ->
    lager:info("[~p] load(~p)~n", [Id, lager:pr(Req, ?MODULE)]),
    timer:sleep(Wait),
    {{ok, Req}, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
