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
	 get/2, 
	 get_data/1,
	 get_class/1,
	 get_scheme/1,
	 get_term/1,
	 get_title/1,
	 get_attributes/1,
	 get_actions/1]).
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type(category_src() :: {mod, atom()} | {xml, term()}).

-record(state, {class      :: atom(),
		data       :: term()}).

%% occi_category callbacks definition
-callback init(occi_category()) -> term().

-callback get(reference(), atom()) -> term().

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
start_link(Ref, Class, Data) ->
    gen_server:start_link({global, Ref}, occi_category, {Class, Data}, []).

-spec new(category_src(), uri()) -> {occi_cid(), reference()} | {error, term()}.
new({mod, Mod}, Location) ->
    Ref = make_ref(),
    Id = occi_mod:get_id(Mod),
    {Class, Data} = case Id#occi_cid.class of
		kind -> 
		    lager:info("Registering kind: ~s -> ~s~n", [occi_renderer:to_uri(Id), Location]),
		    {occi_kind, occi_mod:get_kind(Location, Mod)};
		mixin -> 
		    lager:info("Registering mixin: ~s -> ~s~n", [occi_renderer:to_uri(Id), Location]),
		    {occi_mixin, occi_mod:get_mixin(Location, Mod)}
	    end,
    ChildSpec = {Ref, 
		 {occi_category, start_link, [Ref, Class, Data]}, 
		 permanent, brutal_kill, worker, [occi_category, Class]},
    case supervisor:start_child(occi_category_mgr, ChildSpec) of
	{ok, _Child} ->
	    {Id, Ref};
	{ok, _Child, _Info} ->
	    {Id, Ref};
	{error, Err} ->
	    {error, Err}
    end.

-spec get(reference(), atom()) -> {ok, term()} | {error, term()}.
get(Ref, Name) ->
    gen_server:call({global, Ref}, {get, Name}).

get_class(Ref) ->
    gen_server:call({global, Ref}, {get, class}).

get_scheme(Ref) ->
    gen_server:call({global, Ref}, {get, scheme}).

get_term(Ref) ->
    gen_server:call({global, Ref}, {get, term}).

get_title(Ref) ->
    gen_server:call({global, Ref}, {get, title}).

get_attributes(Ref) ->
    gen_server:call({global, Ref}, {get, attributes}).

get_actions(Ref) ->
    gen_server:call({global, Ref}, {get, actions}).

-spec get_data(reference()) -> occi_category().
get_data(Ref) ->
    gen_server:call({global, Ref}, get_data).

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
init({Class, Data}) ->
    Data2 = Class:init(Data),
    {ok, #state{class=Class, data=Data2}}.

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
handle_call({get, Name}, _From, #state{class=Mod, data=Data}=State) ->
    {Reply, Data2} = Mod:get(Name, Data),
    {reply, Reply, State#state{data=Data2}};
handle_call(get_data, _From, #state{data=Data}=State) ->
    {reply, Data, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
