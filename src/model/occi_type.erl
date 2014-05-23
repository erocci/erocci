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
%%% Created : 10 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_type).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").
-include("occi_xml.hrl").

% Factory for base types
-export([get/1]).

-spec get(atom()) -> function().
get({?xmlschema_ns, string}) ->
    fun to_string/1;

get({?xmlschema_ns, integer}) ->
    fun to_integer/1;

get({?xmlschema_ns, float}) ->
    fun to_float/1;

get({?xmlschema_ns, anyURI}) ->
    fun to_uri/1;

get(_) ->
    {error, invalid_type}.

%%%
%%% Priv
%%%
to_uri(#uri{} = X) ->
    X;
to_uri(X) ->
    occi_uri:parse(X).

to_string(X) when is_list(X) ->
    X;
to_string(X) when is_binary(X) ->
    binary_to_list(X);
to_string(X) ->
    throw({error, {einval, X}}).

to_integer(X) when is_integer(X) ->
    X;
to_integer(X) when is_binary(X) ->
    try binary_to_integer(X) of
	I -> I
    catch
	_:_ ->
	    throw({error, {einval, X}})
    end;
to_integer(X) when is_list(X) ->
    try list_to_integer(X) of
	I -> I
    catch
	_:_ ->
	    throw({error, {einval, X}})
    end;
to_integer(X) ->
    throw({error, {einval, X}}).

to_float(X) when is_float(X) ->
    X;
to_float(X) when is_integer(X) ->
    X+0.0;
to_float(X) when is_binary(X) ->
    try binary_to_float(X) of
	V -> V
    catch 
	_:_ ->
	    try binary_to_integer(X) of
		V -> V+0.0
	    catch
		_:_ -> 
		    throw({error, {einval, X}})
	    end
    end;
to_float(X) when is_list(X) ->
    try list_to_float(X) of
	V -> V
    catch 
	_:_ ->
	    try list_to_integer(X) of
		V -> V+0.0
	    catch
		_:_ -> 
		    throw({error, {einval, X}})
	    end
    end;
to_float(X) ->
    throw({error, {einval, X}}).
