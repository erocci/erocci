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
-module(occi_backend).
-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-record(state, {backend, state}).

%% API
-export([start_link/3]).
-export([save/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% occi_backend callbacks declaration
-type opts() :: [{atom(), any()}].

-callback init(Args :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term()}.

-callback terminate(State :: term()) ->
    term().

-callback save(Obj :: occi_entity(), State :: term()) ->
    {ok, Obj :: occi_entity(), State :: term()} |
    {error, Reason :: term()}.

-callback get(CatId :: occi_cid(), Id :: occi_entity_id(), State :: term()) ->
    {ok, [Entities :: occi_entity()], State :: term()} |
    {nok} |
    {error, Reason :: term()}.

-callback find(CatId :: occi_cid(), Filter :: occi_filter(), State :: term()) ->
    {ok, [Entities :: occi_entity()], term()} |
    {error, Reason :: term()}.

-callback update(CatId :: occi_cid(), Entity :: occi_entity(), State :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term()}.

-callback delete(CatId :: occi_cid(), Id :: occi_entity_id(), State :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term()}.

-callback validate_cfg(opts()) -> opts().

%%%
%%% API
%%% 
-spec start_link(atom(), atom(), term()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Ref, Backend, Opts) ->
    lager:info("Starting storage backend ~p (~p)~n", [Ref, Backend]),
    gen_server:start_link({local, Ref}, ?MODULE, {Backend, Opts}, []).

save(Ref, Entity) ->
    gen_server:call(Ref, {save, Entity}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init({atom(), term()}) -> {ok, term()} | {error, term()} | ignore.
init({Backend, Args}) ->
    case Backend:init(Args) of
	{ok, BackendState} ->
	    {ok, #state{backend=Backend, state=BackendState}};
	{error, Error} ->
	    {stop, Error}
    end.

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
handle_call({save, Obj}, _From, #state{backend=Backend, state=BState}) ->
    {Reply, RState} = Backend:save(Obj, BState),
    {reply, Reply, #state{backend=Backend, state=RState}};
handle_call({get, CatId, Id}, _From, #state{backend=Backend, state=BState}) ->
    {Reply, RState} = Backend:get(CatId, Id, BState),
    {reply, Reply, #state{backend=Backend, state=RState}};
handle_call({find, CatId, Filter}, _From, #state{backend=Backend, state=BState}) ->
    {Reply, RState} = Backend:find(CatId, Filter, BState),
    {reply, Reply, #state{backend=Backend, state=RState}};
handle_call({update, CatId, Entity}, _From, #state{backend=Backend, state=BState}) ->
    {Reply, RState} = Backend:update(CatId, Entity, BState),
    {reply, Reply, #state{backend=Backend, state=RState}};
handle_call({delete, CatId, Id}, _From, #state{backend=Backend, state=BState}) ->
    {Reply, RState} = Backend:delete(CatId, Id, BState),
    {reply, Reply, #state{backend=Backend, state=RState}};
handle_call(Req, From, State) ->
    lager:error("Unknown message from ~p: ~p~n", [From, Req]),
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
terminate(_Reason, #state{backend=Backend, state=State}) ->
    Backend:terminate(State).

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
