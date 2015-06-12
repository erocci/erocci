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

-behaviour(occi_backend).

-include("occi.hrl").

%% occi_backend callbacks
-export([init/1,
	 terminate/1]).
-export([update/2,
	 save/2,
	 delete/2,
	 find/2,
	 load/3,
	 action/3]).

-record(state, {id             :: atom(),
		wait           :: integer()}).

%%%===================================================================
%%% occi_backend callbacks
%%%===================================================================
init(#occi_backend{ref=Ref, opts=Opts}) ->
    {ok, [], #state{id=Ref, wait=proplists:get_value(wait, Opts, 0)}}.

terminate(#state{}) ->
    ok.

save(#state{id=Id, wait=Wait}=State, Obj) when is_record(Obj, occi_node);
		      is_record(Obj, occi_mixin) ->
    ?info("[~p] save(~p)~n", [Id, Obj]),
    timer:sleep(Wait),
    {ok, State}.

delete(#state{id=Id, wait=Wait}=State, Obj) when is_record(Obj, occi_node);
			is_record(Obj, occi_mixin) ->
    ?info("[~p] delete(~p)~n", [Id, Obj]),
    timer:sleep(Wait),
    {ok, State}.

update(#state{id=Id, wait=Wait}=State, #occi_node{}=Node) ->
    ?info("[~p] update(~p)~n", [Id, Node]),
    timer:sleep(Wait),
    {ok, State}.

find(#state{id=Id, wait=Wait}=State, Obj) when is_record(Obj, occi_node);
				    is_record(Obj, occi_mixin) ->
    ?info("[~p] find(~p)~n", [Id, Obj]),
    timer:sleep(Wait),
    {{ok, []}, State}.

load(#state{id=Id, wait=Wait}=State, #occi_node{}=Req, _Opts) ->
    ?info("[~p] load(~p)~n", [Id, ?MODULE]),
    timer:sleep(Wait),
    {{ok, Req}, State}.

action(State, #uri{}=Id, #occi_action{}=A) ->
    ?info("[~p] action(~p, ~p)~n", [?MODULE, Id, A]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
