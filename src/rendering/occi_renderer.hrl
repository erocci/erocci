%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 19 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>

-define(ATOM_TO_BINARY(Value), list_to_binary(atom_to_list(Value))).
-define(ATOM_TO_BINARY(Value, Encoding), list_to_binary(atom_to_list(Value, Encoding))).
