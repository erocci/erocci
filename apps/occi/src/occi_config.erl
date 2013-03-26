%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_config).

%% API
-export([start/0, stop/0, get/1, get/2, set/2]).

-record(config, { key               :: atom(),
									value = undefined :: any() }).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
		Dir = case os:getenv("MNESIA_DIR") of
							false -> os:getenv("HOME") ++ "/.occi/";
							Value -> Value
					end,
		application:set_env(mnesia, dir, Dir),
		case mnesia:create_schema([node()]) 
		of
				ok -> ok;
				{error, {_, {already_exists, _}}} ->
						ok;
				{aborted, _} ->
						erlang:error("Can not create schema")
		end,
		ok = mnesia:start(),
		case mnesia:create_table(config, [{record_name, config},
																			{attributes, record_info(fields, config)},
																			{disc_copies, [node()]},
																			{type, set}])
		of
				{atomic, ok} -> ok;
				{aborted, {already_exists, _}} ->
						ok;
				{aborted, Err} ->
						erlang:error("Can not create table: ~p~n", [Err])
		end,
		mnesia:wait_for_tables([config], 5000),
		ok.

stop() ->
		mnesia:stop().

get(Name) ->
		get(Name, undefined).

get(Name, Default) ->
		F = fun() -> mnesia:read(config, Name) end,
		case mnesia:transaction(F) of
				{atomic, []} ->
						Default;
				{atomic, [ Result ]} ->
						Result;
				{aborted, Reason} ->
						erlang:error("Can not read table config: ~p~n", [Reason])
		end.

set(Name, Value) ->
		F = fun() -> mnesia:write(#config{key=Name, value=Value})	end,
		case mnesia:transaction(F)
		of
				{atomic, ok} -> 
						ok;
				{aborted, Reason} ->
						erlang:error("Can not write in table config: ~p~n", [Reason])
		end.
