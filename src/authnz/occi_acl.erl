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
    check_acls(Acls, {Op, list_to_binary(Path), User}).

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

match({P, '_', '_',            '_'},         {_, _,            _}) ->
    {true, P};
match({P, O,   capabilities,   U},           {O, capabilities, U}) -> 
    {true, P};
match({P, O,   capabilities,   '_'},         {O, capabilities, _}) -> 
    {true, P};
match({P, O,   capabilities,   anonymous},   {O, capabilities, _}) -> 
    {true, P};
match({P, O,   Prefix,         U},           {O, Path,         U}) -> 
    match_path(Prefix, Path, P);
match({P, O,   Prefix,         '_'},         {O, Path,         _}) -> 
    match_path(Prefix, Path, P);
match({P, O,   Prefix,         anonymous},   {O, Path,         _}) -> 
    match_path(Prefix, Path, P);
match({P, O,   '_',            U},           {O, _,            U}) -> 
    {true, P};
match({P, O,   '_',            '_'},         {O, _,            _}) -> 
    {true, P};
match({P, O,   '_',            anonymous},   {O, _,            _}) -> 
    {true, P};
match({P, '_', capabilities,   U},           {_, capabilities, U}) -> 
    {true, P};
match({P, '_', capabilities,   '_'},         {_, capabilities, _}) -> 
    {true, P};
match({P, '_', capabilities,   anonymous},   {_, capabilities, _}) -> 
    {true, P};
match({P, '_', Prefix,         U},           {_, Path,         U}) -> 
    match_path(Prefix, Path, P);
match({P, '_', Prefix,         '_'},         {_, Path,         _}) -> 
    match_path(Prefix, Path, P);
match({P, '_', Prefix,         anonymous},   {_, Path,         _}) -> 
    match_path(Prefix, Path, P);
match({P, '_', '_',            U},           {_, _,            U}) -> 
    {true, P};
match({P, '_', '_',            '_'},         _)                    -> 
    {true, P};
match({P, '_', '_',            anonymous},   _)                    -> 
    {true, P};
match(_,                                     _)                    -> 
    false.

match_path(Prefix, Path, P) ->
    match_path2(filename:split(Prefix), filename:split(Path), P).

match_path2([], _Path, P) ->
    {true, P};
match_path2(_Prefix, [], _P) ->
    false;
match_path2([ H | Prefix ], [ H | Path ], P) ->
    match_path2(Prefix, Path, P);
match_path2([ _H1 | _Prefix ], [ _H2 | _Path ], _P) ->
    false.
