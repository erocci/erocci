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
