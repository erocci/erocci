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
-module(occi_object).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-export([new/2,
	 destroy/1, 
	 save/1,
	 get_attr/2,
	 set_attr/3,
	 set_attrs/2]).
-export([call/3]).

%% fallback methods
-export([impl_abstract/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {handlers         :: [atom()],
		data            :: term()}).

-type(id() :: {Type :: atom(), 
	       Ref  :: pid() | reference() | {local, atom()} | {global, atom()}}).
-export_type([id/0]).

-define(callMethod(Req), element(1, Req)).
-define(callArgs(Req), element(2, Req)).
-define(superData(Data), element(2, Data)).
-define(setSuperData(Data, Val), setelement(2, Data, Val)).
-define(objRef(X), element(2, X)).

%%%
%%% API
%%%
new(Mods, Args) ->
    case gen_server:start_link(occi_object, {Mods, Args}, []) of
	{ok, Pid} -> Pid;
	ignore -> ignore;
	{error, Error} -> {error, Error}
    end.

call(Obj, Name, Args) ->
    Ref = ?objRef(Obj),
    case gen_server:call(Ref, {Name, Args}) of
	ok -> Obj;
	{ok, Reply} -> Reply;
	{error, Err} -> throw({error, Err})
    end.

destroy(Ref) ->
    gen_server:cast(Ref, stop).

save(Ref) ->
    occi_object:call(Ref, impl_save, []).

get_attr(Ref, Name) ->
    occi_object:call(Ref, impl_get_attr, [Name]).

set_attr(Ref, Name, Value) ->
    occi_object:call(Ref, impl_set_attr, [Name, Value]).

set_attrs(Ref, Attributes) ->
    lists:foreach(fun({Name, Value}) ->
			  set_attr(Ref, Name, Value)
		  end, Attributes),
    ok.

%%%
%%% Fallback functions
%%%
impl_abstract(Data) ->
    {{error, notimplemented}, Data}.

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
init({[Mod|Parents], Args}) ->
    Args2 = args_list(Args),
    Data = apply(Mod, init, Args2),
    {ok, #state{handlers=[Mod|Parents], data=Data}}.

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
handle_call(Req, _From, State) ->
    Args = args_list(?callArgs(Req)),
    {Reply, Data2} = call_impl(State#state.handlers, State#state.data, ?callMethod(Req), Args, length(Args)+1),
    {reply, Reply, State#state{data=Data2}}.

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
handle_cast(stop, State) ->
    {stop, normal, State};
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
args_list(Args) when is_tuple(Args) ->
    tuple_to_list(Args);
args_list(Args) when is_list(Args) ->
    Args;
args_list(Args) ->
    [Args].

call_impl([], Data, Name, Args, Arity) ->
    case erlang:function_exported(?MODULE, Name, Arity) of
	true -> 
	    {Ret, SuperData} = apply(?MODULE, Name, [?superData(Data) | Args]),
	    {Ret, ?setSuperData(Data, SuperData)};
	false ->
	    impl_abstract(Data)
    end;
call_impl([Mod|Tail], Data, Name, Args, Arity) ->
    case erlang:function_exported(Mod, Name, Arity) of
	true ->
	    apply(Mod, Name, [Data | Args]);
	false ->
	    {Ret, SuperData} = call_impl(Tail, ?superData(Data), Name, Args, Arity),
	    {Ret, ?setSuperData(Data, SuperData)}
    end.
