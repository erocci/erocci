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
%%% Created : 15 May 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_acl).
-compile({parse_transform, lager_transform}).

-include("occi.hrl").
-include("occi_acl.hrl").

-export([check/3]).

-spec check(acl_op(), acl_node(), acl_user()) -> allow | deny.
check(Op, capabilities, User) ->
    Acls = occi_config:get(acl, []),
    check_acls(Acls, {Op, capabilities, User});
check(Op, #uri{path=Path}, User) ->
    Acls = occi_config:get(acl, []),
    check_acls(Acls, {Op, Path, User}).

%%%
%%% Priv
%%%
check_acls([], _) ->
    % Default policy. Should be configurable ?
    deny;
check_acls([ Acl | Acls ], Action) ->
    case match(Acl, Action) of
	false -> check_acls(Acls, Action);
	{true, Policy} -> Policy
    end.

match({P, O,   N,   U},           {O, N, U}) -> {true, P};
match({P, O,   N,   '_'},         {O, N, _}) -> {true, P};
match({P, O,   N,   anonymous},   {O, N, _}) -> {true, P};
match({P, O,   '_', U},           {O, _, U}) -> {true, P};
match({P, O,   '_', '_'},         {O, _, _}) -> {true, P};
match({P, O,   '_', anonymous},   {O, _, _}) -> {true, P};
match({P, '_', N,   U},           {_, N, U}) -> {true, P};
match({P, '_', N,   '_'},         {_, N, _}) -> {true, P};
match({P, '_', N,   anonymous},   {_, N, _}) -> {true, P};
match({P, '_', '_', U},           {_, _, U}) -> {true, P};
match({P, '_', '_', '_'},         _)         -> {true, P};
match({P, '_', '_', anonymous},   _)         -> {true, P};
match(_,                          _)         -> false.
