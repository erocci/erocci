%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 29 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_entity).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-export([new/2, new/3]).

new(Module, Attributes) ->
    new(Module, <<>>, Attributes).

new(Module, Title, AttrValues) ->
    {occi_attributes, AttrSpecs} = occi_type:get_attributes(Module),
    SpecsDict = lists:foldl(fun({occi_attribute, K, P, F}, Acc) ->
				    dict:store(K, {P, F}, Acc)
			    end, dict:new(), AttrSpecs),
    {Attrs, Errors} = set_attributes(SpecsDict, AttrValues),
    case Errors of
	[] ->
	    {ok, #occi_entity{module=Module, title=Title, attributes=Attrs}};
	L ->
	    {error, L}
    end.

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

set_attributes2(Specs, Values) ->
    lists:foldl(fun({K, V}, {AccAttrs, AccErrors, AccSpecs}) ->
			case set_attribute(K, dict:fetch(K, AccSpecs), V) of
			    {ok, K, V} ->
				{[{K, V}|AccAttrs], AccErrors, dict:erase(K, AccSpecs)};
			    {error, Err} ->
				{AccAttrs, [Err|AccErrors], dict:erase(K, AccSpecs)}
			end
		end, {[], [], Specs}, Values).

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
