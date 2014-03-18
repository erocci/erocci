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
%%% Created : 17 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_cid).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-export([parse/1]).

-spec parse(binary()) -> occi_cid().
parse(Bin) when is_binary(Bin) ->
    case binary:split(Bin, <<"#">>) of
	[Scheme, Term] ->
	    #occi_cid{scheme=to_atom(<< Scheme/binary, "#" >>), term=to_atom(Term), class='_'};
	_ ->
	    throw({error, invalid_cid})
    end.

%%%
%%% Priv
%%%
to_atom(Bin) when is_binary(Bin) ->
    list_to_atom(binary_to_list(Bin)).
