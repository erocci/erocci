%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Sep 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_renderer).

% Some common functions
-export([join/2, to_list/1]).

-callback render(Obj :: term()) ->
    binary() | list().

-callback parse(binary()) ->
    term().

%%%
%%% API
%%%
join(L, Sep) ->
    join(L, [], Sep).

join([], Acc, _Sep) ->
    lists:reverse(Acc);
join([H|[]], Acc, _Sep) ->
    lists:reverse([H|Acc]);
join([H, []|T], Acc, Sep) ->
    join([H|T], Acc, Sep);
join([H|T], Acc, Sep) ->
    join(T, [[H, Sep]|Acc], Sep).

to_list(L) ->
    lists:map(fun(X) when is_atom(X) ->
		      atom_to_list(X);
		 (X) -> X end, L).
