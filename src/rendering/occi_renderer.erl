%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Sep 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_renderer).

-callback render(Obj :: term()) ->
    binary() | list().

-callback parse(binary()) ->
    term().
