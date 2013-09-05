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
-export([join/2, to_list/1, to_uri/1, to_url/1]).

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

to_url(#occi_cid{}=Id) ->
    BaseUrl = occi_config:get(base_location),
    << <<S/binary,$/>> || S <- [BaseUrl|to_uri(Id)] >>.

to_uri(#occi_cid{scheme=Scheme}=Id) when is_atom(Scheme) ->
    to_uri(Id#occi_cid{scheme=list_to_binary(atom_to_list(Scheme))});
to_uri(#occi_cid{}=Id) ->
    [ Id#occi_cid.scheme, list_to_binary(atom_to_list(Id#occi_cid.term)) ];
to_uri(Uri) when is_binary(Uri) ->
    Uri;
to_uri(Uri) when is_atom(Uri) ->
    list_to_binary(atom_to_list(Uri));
to_uri(Uri) when is_list(Uri) ->
    << <<S/binary,$/>> || S <- to_binary(Uri) >>.

to_binary(L) ->
    to_binary(L, []).
to_binary([], Acc) ->
    lists:reverse(Acc);
to_binary([H|T], Acc) when is_atom(H) ->
    to_binary(T, [list_to_binary(atom_to_list(H))|Acc]);
to_binary([H|T], Acc) when is_list(H) ->
    to_binary(T, [list_to_binary(H)|Acc]);
to_binary([H|T], Acc) ->
    to_binary(T, [H|Acc]).


