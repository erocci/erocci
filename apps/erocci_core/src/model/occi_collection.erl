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
%%% Created : 19 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_collection).

-include("occi.hrl").

-export([new/0,
	 new/1,
	 new/2,
	 add_entity/2,
	 add_entities/2,
	 del_entity/2,
	 del_entities/2,
	 get_entities/1,
	 is_empty/1,
	 merge/2,
	 fold/2,
	 add_prefix/2,
	 rm_prefix/2]).

new() ->
    #occi_collection{entities=ordsets:new()}.


new(#uri{}=Id) ->
    #occi_collection{id=Id, entities=ordsets:new()};

new(#occi_cid{}=Cid) ->
    #occi_collection{id=Cid, entities=ordsets:new()}.


new(#uri{}=Id, Elements) when is_list(Elements) ->
    #occi_collection{id=Id, entities=ordsets:from_list(Elements)};

new(#occi_cid{}=Cid, Elements) when is_list(Elements) ->
    #occi_collection{id=Cid, entities=ordsets:from_list(Elements)}.


add_entity(#occi_collection{entities=E}=C, Uri) ->
    C#occi_collection{entities=ordsets:add_element(Uri, E)}.


add_entities(#occi_collection{entities=E}=C, E2) when is_list(E2) ->
    C#occi_collection{entities=ordsets:union(ordsets:from_list(E2), E)}.


del_entity(#occi_collection{entities=E}=C, Uri) ->
    C#occi_collection{entities=ordsets:del_element(Uri, E)}.


del_entities(#occi_collection{entities=E}=C, Uris) ->
    C#occi_collection{entities=ordsets:subtract(E, ordsets:from_list(Uris))}.


get_entities(#occi_collection{entities=E}) ->
    ordsets:to_list(E).


is_empty(#occi_collection{entities=E}) ->
    case ordsets:size(E) of
	0 -> true;
	_ -> false
    end.

merge(#occi_collection{}=C, undefined) ->
    C;
merge(#occi_collection{id=Id, entities=E1}=C1,
      #occi_collection{id=Id, entities=E2}) ->
    C1#occi_collection{entities=ordsets:union(E1, E2)};
merge(_C1, _C2) ->
    throw({error, merge_collection_failed}).

fold(#occi_collection{entities=E}=C, F) when is_function(F) ->
    E2 = ordsets:fold(fun (Uri, Acc) ->
			      ordsets:add_element(F(Uri), Acc)
		      end, ordsets:new(), E),
    C#occi_collection{entities=E2}.

-spec add_prefix(occi_collection(), string()) -> occi_collection().
add_prefix(#occi_collection{}=Coll, Prefix) when is_list(Prefix) ->
    fold(Coll, fun (#uri{}=Uri) -> 
		       occi_uri:add_prefix(Uri, Prefix);
		   (#occi_node{}=Node) ->
		       occi_node:add_prefix(Node, Prefix)
	       end).

-spec rm_prefix(occi_collection(), string()) -> occi_collection().
rm_prefix(#occi_collection{}=Coll, Prefix) when is_list(Prefix) ->
    fold(Coll, fun (#uri{}=Uri) -> 
		       occi_uri:rm_prefix(Uri, Prefix);
		   (#occi_node{}=Node) ->
		       occi_node:rm_prefix(Node, Prefix)
	       end).
