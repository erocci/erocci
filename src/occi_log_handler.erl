%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
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
%%% Created :  4 March 2015 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_log_handler).

-behaviour(gen_event).

%% gen_event callback
-export([init/1,
	 handle_event/2,
	 handle_call/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-define(MONTHS, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}).
-define(MSG(Lvl, Pid, Format, Data), io:format("=[" ++ Lvl ++ "]=" ++ get_prefix(Pid) ++ Format ++ "~n", Data)).
-define(REPORT(Lvl, Pid, Report), 
	io:format("=[" ++ Lvl ++ " REPORT]=" ++ get_prefix(Pid) ++ "~n", []), io:format("~p~n", [Report])).

-spec init(Args :: term()) -> {ok, State :: term()} | 
			      {ok, State :: term(), hibernate} |
			      {error, Reason :: term()}.
init(_Args) ->
    {ok, []}.


-spec handle_event(Event :: term(), State :: term()) -> 
			  {ok, NewState :: term()} |
			  {ok, NewState :: term(), hibernate} |
			  {swap_handler, Args1 :: term(), NewState :: term(), Handler2 :: atom() | {atom(), term()}, Args2 :: term()} |
			  remove_handler.
handle_event({error, _Gleader, {Pid, Format, Data}}, State) ->
    ?MSG("ERROR", Pid, Format, Data),
    {ok, State};
handle_event({error_report, _Gleader, {Pid, _Type, Report}}, State) ->
    ?REPORT("ERROR", Pid, Report),
    {ok, State};
handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State) ->
    ?MSG("WARN", Pid, Format, Data),
    {ok, State};
handle_event({warning_report, _Gleader, {Pid, _Type, Report}}, State) ->
    ?REPORT("WARN", Pid, Report),
    {ok, State};
handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State) ->
    ?MSG("INFO", Pid, Format, Data),
    {ok, State};
handle_event({info_report, _Gleader, {Pid, _Type, Report}}, State) ->
    ?REPORT("INFO", Pid, Report),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.


-spec handle_call(Request :: term(), State :: term()) ->
			  {ok, Reply :: term(), NewState :: term()} |
			  {ok, Reply :: term(), NewState :: term(), hibernate} |
			  {swap_handler, Reply :: term(), Args1 :: term(), NewState :: term(), Handler2 :: atom() | {atom(), term()}, Args2 :: term()} |
			  {remove_handler, Reply :: term()}.
handle_call(_Request, State) ->
    {ok, [], State}.


-spec handle_info(Info :: term(), State :: term()) ->
			 {ok, NewState :: term()} | 
			 {ok, NewState :: term(), hibernate} |
			 {swap_handler, Args1 :: term(), NewState :: term(), Handler2 :: atom() | {atom(), term()}, Args2 :: term()} | 
			 remove_handler.
handle_info(_Info, State) ->
    {ok, State}.


-spec terminate(Arg :: term(), State :: term()) -> term().
terminate(_Arg, _State) ->
    ok.


-spec code_change(OldVsn :: term() | {down, term()}, State :: term(), Extra :: term()) -> {ok, NewState :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Priv
%%%
get_prefix(_Pid) ->
    {{Y,Mth,D},{H,M,S}} = erlang:localtime(),
    io_lib:format("=~4b-~s-~2..0b ~2..0b:~2..0b:~2..0b== ", [Y,element(Mth,?MONTHS),D,H,M,S]).
