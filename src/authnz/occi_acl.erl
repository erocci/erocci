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

-export([check/3]).

-spec check(acl_op(), occi_node(), acl_user()) -> allow | deny.
check(Op, Node, User) ->
    Acls = occi_config:get(acl, []),
    check_acls(Acls, {Op, Node, User}).

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

-define(caps_node, #occi_node{type=capabilities}).

match({P, '_', '_',            '_'},         {_, _,            _}) ->
    {true, P};

match({P, '_', '_',            U},           {_, N,            User}) -> 
    match_user(U, N, User, P);

match({P, '_', capabilities,   '_'},         {_, ?caps_node,   _}) -> 
    {true, P};
match({P, '_', Prefix,         '_'},         {_, Node,         _}) -> 
    match_path(Prefix, Node, P);

match({P, O1,  '_',            '_'},         {O2, _,           _}) -> 
    match_op(O1, O2, P);

match({P, '_', capabilities,   U},           {_, ?caps_node=N, User}) -> 
    match_user(U, N, User, P);
match({P, '_', Prefix,         U},           {_, N,            User}) ->
    match_and([{fun match_path/3, [Prefix, N, P]},
	       {fun match_user/4, [U, N, User, P]}], P);

match({P, O1,  '_',            U},           {O2,N,            User}) -> 
    match_and([{fun match_op/3, [O1, O2, P]},
	       {fun match_user/4, [U, N, User, P]}], P);

match({P, O1,  capabilities,   '_'},         {O2,?caps_node,   _}) -> 
    match_op(O1, O2, P);
match({P, O1,  Prefix,         '_'},         {O2, Node,         _}) -> 
    match_and([{fun match_op/3, [O1, O2, P]},
	       {fun match_path/3, [Prefix, Node, P]}], P);

match({P, O1,  capabilities,   U},           {O2, ?caps_node=N, User}) ->
    match_and([{fun match_op/3, [O1, O2, P]},
	       {fun match_user/4, [U, N, User, P]}], P);
match({P, O1,  Prefix,         U},           {O2,N,            User}) ->
    match_and([{fun match_op/3, [O1, O2, P]},
	       {fun match_path/3, [Prefix, N, P]},
	       {fun match_user/4, [U, N, User, P]}], P);

match(_,                                     _)                    -> 
    false.

match_and([], P) ->
    {true, P};
match_and([ {Fun, Args} | Funs ], P) ->
    case erlang:apply(Fun, Args) of
	{true, _P} -> 
	    match_and(Funs, P);
	false ->
	    false
    end.

match_op(update, {action, _}, P) ->
    {true, P};
match_op(O, O, P) ->
    {true, P};
match_op(_, _, _) ->
    false.

match_user(anonymous, _N, _U, P) ->
    {true, P};
match_user(authenticated, _N, anonymous, _) ->
    false;
match_user(authenticated, _N, _U, P) ->
    {true, P};
match_user(admin, _N, admin, P) ->
    {true, P};
match_user(admin, _N, _U, _P) ->
    false;
match_user(owner, #occi_node{owner=U}, U, P) ->
    {true, P};
match_user(owner, _N, _U, _P) ->
    false;
match_user(group, #occi_node{owner=O}, O, P) ->
    {true, P};
match_user(group, #occi_node{owner=O}, U, P) ->
    case epasswd:share_group(O, U) of
	true -> {true, P};
	false -> false
    end;
match_user(_, _, _, _) ->
    false.

match_path(Prefix, #occi_node{id=#uri{path=Path}}, P) ->
    match_path2(filename:split(Prefix), filename:split(Path), P).

match_path2([], _Path, P) ->
    {true, P};
match_path2(_Prefix, [], _P) ->
    false;
match_path2([ H | Prefix ], [ H | Path ], P) ->
    match_path2(Prefix, Path, P);
match_path2([ _H1 | _Prefix ], [ _H2 | _Path ], _P) ->
    false.
