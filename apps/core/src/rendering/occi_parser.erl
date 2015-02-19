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
%%% @doc An event based JSON parser
%%%
%%% @end
%%% Created : 11 Dec 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_parser).

-include("occi_log.hrl").
-include("occi_parser.hrl").

-export([send_event/3,
	 parse_error/2]).

parse_error(#token{}=Token, Ctx) ->
    ?error(build_err(Token)),
    {reply, {error, parse_error}, eof, Ctx}.

build_err(#token{name=Name, pos=undefined, data=undefined}) ->
    io_lib:format("Invalid term: ~p~n", [Name]);
build_err(#token{name=Name, pos=undefined, data=Data}) ->
    io_lib:format("Invalid term: ~p(~p)~n", [Name, Data]);
build_err(#token{name=Name, pos=Pos, data=undefined}) ->
    io_lib:format("Invalid term at line ~p: ~p~n", [Pos, Name]);
build_err(#token{name=Name, pos=Pos, data=Data}) ->
    io_lib:format("Invalid term at line ~p: ~p(~p)~n", [Pos, Name, Data]).

send_event(_Event, IfOk, #parser{sink=undefined}) ->
    IfOk;
send_event(Event, IfOk, #parser{sink=Sink}=Ctx) ->
    Res = case Event of
	      eof ->
		  stop_parser(Sink);
	      Else ->
		  gen_fsm:sync_send_event(Sink#parser.id, Else)
	  end,
    case Res of
	ok ->
	    IfOk;
	{eof, Result} ->
	    stop_parser(Sink),
	    {reply, {eof, Result}, eof, Ctx};
	{error, Reason} ->
	    stop_parser(Sink),
	    {reply, {error, Reason}, eof, Ctx}
    end.

stop_parser(#parser{id=Ref}) ->
    try gen_fsm:sync_send_all_state_event(Ref, stop) of
	ok ->
	    ok
    catch
	exit:{normal, _} ->
	    ok;
	exit:{noproc, _} ->
	    ok;
	_:Err ->
	    {error, Err}
    end.
