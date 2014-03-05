%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
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
%%% Created :  3 Sep 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_renderer).

-include("occi.hrl").

% Some common functions
-export([join/2, to_list/1]).

-callback render(Node :: occi_node(), Env :: any()) ->   % text/occi rendering needs HTTP request, beurk !!!
    binary() | list().

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
