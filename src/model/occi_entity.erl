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
%%% Created : 29 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_entity).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-export([save/1]).
-export([get_cid/1,
	 get_id/1,
	 set_id/2,
	 set_attributes/2, 
	 set_attribute/3]).

%%%
%%% API
%%%
save(Entity) when is_record(Entity, occi_resource); 
		  is_record(Entity, occi_link) ->
    Cid = get_cid(Entity),
    Backend = occi_store:get_backend(Cid),
    occi_backend:save(Backend, Entity).

get_cid(#occi_resource{cid=Cid}) ->
    Cid;
get_cid(#occi_link{cid=Cid}) ->
    Cid.

-spec get_id(occi_entity()) -> uri().
get_id(#occi_resource{id=Id}) ->
    Id;
get_id(#occi_link{id=Id}) ->
    Id.

-spec set_id(occi_entity(), uri()) -> occi_entity().
set_id(#occi_resource{}=Entity, Id) ->
    Entity#occi_resource{id=Id};
set_id(#occi_link{}=Entity, Id) ->
    Entity#occi_link{id=Id}.

set_attributes(Specs, Values) ->
    {Attrs, Errors, Specs2} = set_attributes2(Specs, Values),
    % TODO: check spec coherence
    % If attribute is immutable, it can not be set by user ?
    % Do not check it, so...
    Specs3 = dict:filter(fun(_K, {P, _Cb}) ->
			       not lists:member(immutable, P)
		       end, Specs2),
    % Check remaining attributes are not required
    Errors2 = dict:fold(fun(K, {P, _Cb}, Acc) ->
				case lists:member(required, P) of
				    true -> [{einval, K}|Acc];
				    false -> Acc
				end
			end, Errors, Specs3),
    {Attrs, Errors2}.

set_attribute(K, {P, {M, F, Args}}, Obj) ->
    case lists:member(immutable, P) of
	true ->
	    {error, {einval, K}};
	false ->
	    Ret = case Args of
		      [] -> M:F(Obj);
		      L -> M:F(Obj, L)
		  end,
	    case Ret of
		{ok, Val} ->
		    {ok, K, Val};
		error ->
		    {error, {einval, K}}
	    end
    end.

%%%
%%% Private
%%%
set_attributes2(Specs, Values) ->
    lists:foldl(fun({K, V}, {AccAttrs, AccErrors, AccSpecs}) ->
			case set_attribute(K, dict:fetch(K, AccSpecs), V) of
			    {ok, K, V} ->
				{[{K, V}|AccAttrs], AccErrors, dict:erase(K, AccSpecs)};
			    {error, Err} ->
				{AccAttrs, [Err|AccErrors], dict:erase(K, AccSpecs)}
			end
		end, {[], [], Specs}, Values).
