%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
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
%%% Created : 27 May 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_authnz).

-behaviour(gen_server).

%% API
-export([start_link/2,
         auth/2,
         share_group/3,
         create_group/2,
         create_ingroup/4,
         delete_group/2,
         delete_ingroup/2,
         update_group/3,
         update_user/3, delete_user/2, create_user/3, get_groups/1, start/2,
         get_group/2, get_user/2, get_users/1,
         get_ingroup/2, get_ingroups/1, get_group_user/2]).

-callback init(Opts :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term()}.

-callback start(Opts :: term(), State :: term()) ->
    true | false.

-callback auth(Credentials :: term(), State :: term()) ->
    true | false.

-callback share_group(User1 :: term(), User2 :: term(), State :: term()) ->
    true | false.

-callback create_group(Group :: term(), State :: term()) ->
    ok | {error, term()}.

-callback delete_group(Group :: term(), State :: term()) ->
    ok | {error, term()}.

-callback create_ingroup(Idingroup :: term(),User :: term(), Group :: term(), State :: term()) ->
    ok | {error, term()}.

-callback delete_ingroup(InGroup :: term(), State :: term()) ->
    ok | {error, term()}.

-callback update_group(Group :: term(), NGroup :: term(), State :: term()) ->
    ok | {error, term()}.

-callback update_user(Jid :: term(), User :: term(), State :: term()) ->
    ok | {error, term()}.

-callback create_user(Jid :: term(), User :: term(), State :: term()) ->
    ok | {error, term()}.

-callback delete_user(User :: term(), State :: term()) ->
    ok | {error, term()}.

-callback get_groups(State :: term()) ->
    term().

-callback get_group(Group :: term(), State :: term()) ->
    term() | {error, term()}.

-callback get_users(State :: term()) ->
    term().

-callback get_user(User :: term(), State :: term()) ->
    term() | {error, term()}.

-callback get_ingroups(State :: term()) ->
    term().

-callback get_ingroup(Ingroup :: term(), State :: term()) ->
    term() | {error, term()}.

-callback get_group_user(User :: term(), State :: term()) ->
     term().

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {mod      :: atom(),
                state    :: term()}).

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
start_link(Mod, Opts) ->
    occi_authnz_sup:add_child(Mod, Opts).

-spec auth(reference(), term()) -> true | false.
auth(Ref, Credentials) ->
    gen_server:call(Ref, {auth, Credentials}).

-spec share_group(reference(), term(), term()) -> true | false.
share_group(Ref, User1, User2) ->
    gen_server:call(Ref, {share_group, User1, User2}).

-spec create_group(reference(), term()) -> ok | {error, term()}.
create_group(Ref, Group) ->
    gen_server:call(Ref, {create_group, Group}).

-spec delete_group(reference(), term()) -> ok | {error, term()}.
delete_group(Ref, Group) ->
    gen_server:call(Ref, {delete_group, Group}).

-spec create_ingroup(reference(), term(), term(), term()) -> ok | {error, term()}.
create_ingroup(Ref, Idingroup, User, Group) ->
    gen_server:call(Ref, {create_ingroup, Idingroup, User, Group}).

-spec delete_ingroup(reference(), term()) -> ok | {error, term()}.
delete_ingroup(Ref, InGroup) -> 
    gen_server:call(Ref, {delete_ingroup, InGroup}).

-spec update_group(reference(), term(), term()) -> ok | {error, term()}.
update_group(Ref, Group, NGroup) ->
    gen_server:call(Ref, {update_group, Group, NGroup}).

-spec create_user(reference(), term(), term()) -> ok | {error, term()}.
create_user(Ref, Jid, User) ->
    gen_server:call(Ref, {create_user, Jid, User}).

-spec delete_user(reference(), term()) -> ok | {error, term()}.
delete_user(Ref, User) ->
    gen_server:call(Ref, {delete_user, User}).

-spec update_user(reference(), term(), term()) -> ok | {error, term()}.
update_user(Ref, Jid, User) ->
    gen_server:call(Ref, {update_user, Jid, User}).

-spec get_groups(reference()) -> term().
get_groups(Ref) -> 
    gen_server:call(Ref, {get_groups}).

-spec get_users(reference()) -> term().
get_users(Ref) -> 
    gen_server:call(Ref, {get_users}).

-spec get_user(reference(), term()) -> term() | {error, term()}.
get_user(Ref, User) -> 
    gen_server:call(Ref, {get_user, User}).

-spec get_group(reference(), term()) -> term() | {error, term()}.
get_group(Ref, Group) -> 
    gen_server:call(Ref, {get_group, Group}).

-spec get_ingroups(reference()) -> term().
get_ingroups(Ref) -> 
    gen_server:call(Ref, {get_ingroups}).

-spec get_ingroup(reference(), term()) -> term() | {error, term()}.
get_ingroup(Ref, InGroup) -> 
    gen_server:call(Ref, {get_ingroup, InGroup}).

-spec start(reference(), term()) -> ok | {error, term()}.
start(Ref, Opts) -> 
    gen_server:call(Ref, {start, Opts}).

-spec get_group_user(reference(), term()) -> term().
get_group_user(Ref, User) -> 
    gen_server:call(Ref, {get_group_user, User}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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
init({Mod, Opts}) ->
    try Mod:init(Opts) of
	State -> 
	    {ok, #state{mod=Mod, state=State}}
    catch throw:Err ->
	    {stop, Err}
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
handle_call({auth, Credentials}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:auth(Credentials, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({share_group, User1, User2}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:share_group(User1, User2, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;    
handle_call({create_group, Group}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:create_group(Group, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({delete_group, Group}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:delete_group(Group, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({create_ingroup, Idingroup, User, Group}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:create_ingroup(Idingroup, User, Group, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({delete_ingroup, InGroup}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:delete_ingroup(InGroup, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({update_group, Group, NGroup}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:update_group(Group, NGroup, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({update_user, Jid, User}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:update_user(Jid, User, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({create_user, Jid, User}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:create_user(Jid, User, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({delete_user, User}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:delete_user(User, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({get_groups}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:get_groups(S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({get_users}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:get_users(S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({get_group, Group}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:get_group(Group, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({get_user, User}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:get_user(User, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({start, Opts}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:start(Opts, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({get_ingroups}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:get_ingroups(S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({get_ingroup, Ingroup}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:get_ingroup(Ingroup, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
handle_call({get_group_user, User}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:get_group_user(User, S) of
        {Ret, S2} ->
            {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
              {stop, Err, State}
    end;
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
