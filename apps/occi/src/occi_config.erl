%%% @author Jean Parpaillon <jean@bison.home>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 23 Mar 2013 by Jean Parpaillon <jean@bison.home>

-module(occi_config).

%% API
-export([get_env/1, get_env/2]).

get_env(Name) ->
		get_env(Name, undefined).

get_env(Name, Default) -> 
		case os:getenv(Name) of
				false ->
						case Default of
								undefined ->
										erlang:error({undefined, "Undefined " ++ Name});
								_ ->
										Default
						end;				
				Value -> Value
		end.

