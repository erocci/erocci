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
%%% Created : 5 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_parser_occi).
-compile({parse_transform, lager_transform}).

-include("occi_parser_text.hrl").

%% API
-export([parse_action/3,
	 parse_entity/3,
	 parse_user_mixin/2,
	 parse_collection/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_action(_, Req, Action) ->
    case parse_full(Req, #state{action=Action}) of
	{error, Reason} ->
	    {error, {parse_error, Reason}};
	{ok, #occi_request{action=#occi_action{}=Action2}} ->
	    {ok, Action2};
	_ ->
	    {error, {parse_error, not_an_action}}
    end.    

parse_entity(_, Req, #occi_resource{}=Res) ->
    case parse_full(Req, #state{entity=Res}) of
	{error, Reason} ->
	    {error, {parse_error, Reason}};
	{ok, #occi_request{entities=[#occi_resource{}=Res2]}} ->
	    {ok, Res2};
	_ ->
	    {error, {parse_error, not_an_entity}}
    end;

parse_entity(_, Req, #occi_link{}=Link) ->
    case parse_full(Req, #state{entity=Link}) of
	{error, Reason} ->
	    {error, {parse_error, Reason}};
	{ok, #occi_request{entities=[#occi_link{}=Link2]}} ->
	    {ok, Link2};
	_ ->
	    {error, {parse_error, not_an_entity}}
    end;

parse_entity(_, Req, #occi_entity{id=Id}) ->
    case parse_full(Req, #state{entity_id=Id}) of
	{error, Reason} ->
	    {error, {parse_error, Reason}};
	{ok, #occi_request{entities=[#occi_resource{}=Res2]}} ->
	    {ok, Res2};
	{ok, #occi_request{entities=[#occi_link{}=Link2]}} ->
	    {ok, Link2};
	_ ->
	    {error, {parse_error, not_an_entity}}
    end.

parse_user_mixin(_, Req) ->
    case parse_full(Req, #state{mixin=occi_mixin:new(#occi_cid{class=usermixin})}) of
	{error, Reason} ->
	    {error, {parse_error, Reason}};
	{ok, #occi_request{mixins=[#occi_mixin{}=Mixin]}} ->
	    {ok, Mixin};
	Err ->
	    lager:error("Invalid request: ~p~n", [Err]),
	    {error, {parse_error, Err}}
    end.

parse_collection(_, Req) ->
    case parse_full(Req, #state{}) of
	{error, Reason} ->
	    {error, {parse_error, Reason}};
	{ok, #occi_request{collection=Coll}} ->
	    {ok, Coll}
    end.

%%%
%%% Private
%%%
parse_full(Req, _State) ->
    {Headers, _} = cowboy_req:headers(Req),
    lager:info("### Headers: ~p~n", [Headers]),
    {error, not_yet}.
