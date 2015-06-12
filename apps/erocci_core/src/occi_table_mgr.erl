%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2014 Jean Parpaillon.
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

%% @doc 
-module(occi_table_mgr).

-include("occi_log.hrl").

-export([start_link/0,
	 new/2,
	 delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {tables}).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    ?info("Starting OCCI table manager~n", []),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec new(atom(), [term()]) -> {error, unauthorized} | {ok, term()}.   % tid()
new(Name, Opts) ->
    gen_server:call(?SERVER, {new, Name, Opts}).

-spec delete(atom()) -> ok | {error, unauthorized}.
delete(Tid) ->
    ets:delete(Tid),
    gen_server:call(?SERVER, {delete, Tid}).

%%%
%%% gen_server callbacks
%%%

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
-spec init([]) -> {ok, term()} | {error, term()} | ignore.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{tables=dict:new()}}.

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
handle_call({new, Name, Opts}, {Pid, _Tag}, #state{tables=Tables}=State) ->
    case erlang:process_info(Pid, registered_name) of
	[] ->
	    {reply, {error, unauthorized}, State};
	{registered_name, RegName} ->
	    case dict:find(Name, Tables) of
		error ->
		    link(Pid),
		    T = ets:new(Name, Opts),
		    ets:setopts(T, {heir, self(), []}),
		    ets:give_away(T, Pid, []),
		    {reply, {ok, T}, State#state{tables=dict:store(Name, {T, RegName}, Tables)}};
		{ok, {T, RegName}} -> 
		    ets:give_away(T, Pid),
		    {reply, {ok, T}, State};
		{ok, {_T, _OtherName}} ->
		    {reply, {error, unauthorized}, State}
	    end
    end;

handle_call({delete, Tid}, {Pid, _Tag}, #state{tables=Tables}=State) ->
    case erlang:process_info(Pid, registered_name) of
	[] ->
	    {reply, {error, unauthorized}, State};
	{registered_name, RegName} ->
	    case [ K || {K, {V1, V2}} <- dict:to_list(Tables), Tid =:= K orelse Tid =:= V1, RegName =:= V2 ] of
		[] -> 
		    {reply, {error, unauthorized}, State};
		[TableName] ->
		    {reply, ok, State#state{tables=dict:erase(TableName, Tables)}}
	    end
    end;

handle_call(Req, From, State) ->
    ?error("Unknown message from ~p: ~p~n", [From, Req]),
    {reply, ok, State}.

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
handle_info({'ETS-TRANSFER', TableId, OldOwner, _Data}, State) ->
    ?debug("Receive 'ETS-TRANSFER' for ~p from ~p~n", [TableId, OldOwner]),
    {noreply, State};

handle_info({'EXIT', From, Reason}, State) ->
    ?debug("Table owner died ~p: ~p~n", [From, Reason]),
    {noreply, State};

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
