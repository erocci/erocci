%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>

-record(config, {key :: any(), value :: any()}).
-type config() :: #config{}.

-record(state, { opts = [] :: [config()] }).
