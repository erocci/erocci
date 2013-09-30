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

-behaviour(gen_server).

-include("occi.hrl").

%% API
-export([new/2, 
	 get_attr/2, 
	 get_collection/1,
	 get_obj/1]).
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type(category_src() :: {mod, atom()} | {xml, term()}).

-record(state, {handler    :: atom(),
		data       :: term()}).

%% occi_category callbacks definition
-callback init(occi_store:backend_ref(), occi_category()) -> term().

-callback get_attr(reference(), atom()) -> term().

-callback get_obj(reference()) -> occi_category().

-callback get_collection(reference()) -> [occi_entity()].

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(reference(), atom(), occi_category()) -> {ok, pid()} | {error, term()}.
start_link(Ref, Handler, Data) ->
    gen_server:start_link({global, Ref}, occi_category, {Handler, Data}, []).

-spec new(category_src(), uri()) -> {occi_cid(), reference()} | {error, term()}.
new({mod, Mod}, Location) ->
    Category = occi_mod:get_category(Location, Mod),
    register_category(Category).

-spec get_attr(reference(), atom()) -> {ok, term()} | {error, term()}.
get_attr(Ref, Name) ->
    gen_server:call({global, Ref}, {get_attr, Name}).

-spec get_collection(reference()) -> [occi_entity()].
get_collection(Ref) ->
    gen_server:call({global, Ref}, get_collection).

-spec get_obj(reference()) -> occi_category().
get_obj(Ref) ->
    gen_server:call({global, Ref}, get_obj).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%%--------------------------------------------------------------------
-spec init({atom(), occi_category()}) -> {ok, #state{}} | {error, term()}.
init({Handler, Obj}) ->
    Backend = occi_store:get_backend(get_location(Obj)),
    Data = Handler:init(Backend, Obj),
    {ok, #state{handler=Handler, data=Data}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_attr, Name}, _From, #state{handler=Mod, data=Data}=State) ->
    Reply = Mod:get_attr(Name, Data),
    {reply, Reply, State};
handle_call(get_obj, _From, #state{handler=Mod, data=Data}=State) ->
    Obj = Mod:get_obj(Data),
    {reply, Obj, State};
handle_call(get_collection, _From, #state{handler=Mod, data=Data}=State) ->
    Entities = Mod:get_collection(Data),
    {reply, Entities, State};
handle_call(Request, From, State) ->
    lager:error("Unknown message from ~p: ~p~n", [From, Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
register_category(Category) ->
    Id = get_id(Category),
    Handler = get_handler(Category),
    Ref = make_ref(),
    ChildSpec = {Ref, 
		 {occi_category, start_link, [Ref, Handler, Category]}, 
		 permanent, brutal_kill, worker, [occi_category, Handler]},
    case supervisor:start_child(occi_category_mgr, ChildSpec) of
	{ok, _Child} ->
	    {Id, Ref};
	{ok, _Child, _Info} ->
	    {Id, Ref};
	{error, Err} ->
	    {error, Err}
    end.

get_id(#occi_kind{id=Id}) ->
    Id;
get_id(#occi_mixin{id=Id}) ->
    Id.

get_location(#occi_kind{location=Location}) ->
    Location;
get_location(#occi_mixin{location=Location}) ->
    Location.

get_handler(#occi_kind{}=_Obj) ->
    occi_kind;
get_handler(#occi_mixin{}=_Obj) ->
    occi_mixin.
