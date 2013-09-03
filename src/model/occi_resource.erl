%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 30 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_resource).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-export([new/2, new/3, new/5,
	 save/1]).

%%%
%%% API
%%%
new(Module, Attributes) ->
    new(Module, undefined, <<>>, <<>>, Attributes).

new(Module, Id, Attributes) ->
    new(Module, Id, <<>>, <<>>, Attributes).

new(Module, Id, Title, Summary, AttrValues) ->
    {occi_attributes, AttrSpecs} = occi_type:get_attributes(Module),
    SpecsDict = lists:foldl(fun({occi_attribute, K, P, F}, Acc) ->
				    dict:store(K, {P, F}, Acc)
			    end, dict:new(), AttrSpecs),
    {Attrs, Errors} = occi_entity:set_attributes(SpecsDict, AttrValues),
    case Errors of
	[] ->
	    Cid = occi_type:get_id(Module),
	    {ok, #occi_resource{id=Id, cid=Cid, title=Title, summary=Summary, attributes=Attrs}};
	L ->
	    {error, L}
    end.

save(#occi_resource{}=Resource) ->
    occi_entity:save(Resource).
