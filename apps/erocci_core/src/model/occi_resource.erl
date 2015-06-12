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

-include("occi.hrl").

-export([new/0,
         new/1,
         new/2,
         new/4,
         get_id/1,
         set_id/2,
         get_cid/1,
         set_cid/2,
         get_mixins/1,
         add_mixin/2,
         del_mixin/2,
         set_attr_value/3,
         get_attr/2,
         get_attr_value/2,
         get_attributes/1,
         add_link/2,
         get_links/1,
         get_links_size/1,
         add_prefix/2,
         rm_prefix/2,
         update_attr_value/2,
	 has_category/2,
	 match_attr/3]).

-export([reset/1]).

-define(CORE_ATTRS, [{'occi.core.title', occi_attribute:core_title()},
		     {'occi.core.summary', occi_attribute:core_summary()}]).

%%%
%%% API
%%%
-spec new() -> occi_resource().
new() ->
    #occi_resource{attributes=orddict:from_list(?CORE_ATTRS), links=sets:new()}.

-spec new(occi_kind() | term()) -> occi_resource().
new(#occi_kind{}=Kind) ->
    Attrs = [orddict:to_list(occi_kind:get_attributes(Kind)),
	     ?CORE_ATTRS],
    #occi_resource{cid=occi_kind:get_id(Kind), 
                   attributes=orddict:from_list(lists:flatten(Attrs)),
                   links=sets:new()};
new(Id) ->
    #occi_resource{id=Id, 
		   attributes=orddict:from_list(?CORE_ATTRS), 
		   links=sets:new()}.

-spec new(Id :: occi_objid(), Kind :: occi_kind()) -> occi_resource().
new(Id, #occi_kind{}=Kind) ->
    Attrs = [orddict:to_list(occi_kind:get_attributes(Kind)),
	     ?CORE_ATTRS],
    #occi_resource{id=Id, cid=occi_kind:get_id(Kind), 
                   attributes=orddict:from_list(lists:flatten(Attrs)),
                   links=sets:new()}.

-spec new(Id :: occi_objid(), 
	  Kind :: occi_kind(), 
	  Mixins :: [occi_mixin()], 
	  Attributes :: [{Key :: atom(), Val :: term}]) -> occi_resource().
new(Id, #occi_kind{}=Kind, Mixins, Attributes) ->
    Attrs = [?CORE_ATTRS,
	     orddict:to_list(occi_kind:get_attributes(Kind)),
	     lists:map(fun (Mixin) ->
			       orddict:to_list(occi_kind:get_attributes(Mixin))
		       end, Mixins)],
    R = #occi_resource{id=Id, cid=occi_kind:get_id(Kind), 
                       attributes=orddict:from_list(lists:flatten(Attrs)),
                       links=sets:new()},
    lists:foldl(fun ({Key, Value}, Acc) ->
                         occi_resource:set_attr_value(Acc, Key, Value)
                end, R, Attributes).

-spec get_id(occi_resource()) -> uri().
get_id(#occi_resource{id=Id}) ->
    Id.

-spec set_id(occi_resource(), occi_objid() | binary()) -> occi_resource().
set_id(#occi_resource{}=Res, Id) ->
    Res#occi_resource{id=Id}.

-spec get_cid(occi_resource()) -> occi_cid().
get_cid(#occi_resource{cid=Cid}) ->
    Cid.

-spec set_cid(occi_resource(), occi_kind()) -> occi_resource().
set_cid(#occi_resource{attributes=Attrs}=Res, #occi_kind{id=Cid}=Kind) ->
    Attrs2 = orddict:merge(fun (_Key, _Val1, Val2) ->
                                    Val2
                           end, Attrs, occi_kind:get_attributes(Kind)),
    Res#occi_resource{cid=Cid, attributes=Attrs2}.

-spec get_mixins(occi_resource()) -> term(). % set()
get_mixins(#occi_resource{mixins=undefined}) ->
    sets:new();
get_mixins(#occi_resource{mixins=Mixins}) ->
    Mixins.

-spec add_mixin(occi_resource(), occi_mixin()) -> occi_resource().
add_mixin(#occi_resource{mixins=undefined}=Res, Mixin) ->
    add_mixin(Res#occi_resource{mixins=sets:new()}, Mixin);
add_mixin(#occi_resource{mixins=Mixins, attributes=Attrs}=Res, #occi_mixin{id=Cid}=Mixin) ->
    Res#occi_resource{mixins=sets:add_element(Cid, Mixins),
                      attributes=occi_entity:merge_attrs(Mixin, Attrs)}.

-spec del_mixin(occi_resource(), occi_mixin()) -> occi_resource().
del_mixin(#occi_resource{mixins=undefined}=Res, _) ->
    Res;
del_mixin(#occi_resource{mixins=Mixins, attributes=Attrs}=Res, 
          #occi_mixin{id=Cid}=Mixin) ->
    Res#occi_resource{mixins=sets:del_element(Cid, Mixins), 
                      attributes=occi_entity:rm_attrs(Mixin, Attrs)}.

-spec set_attr_value(occi_resource(), occi_attr_key(), any()) -> occi_resource().
set_attr_value(#occi_resource{}=Res, 'occi.core.id', Val) ->
    set_id(Res, Val);
set_attr_value(#occi_resource{attributes=Attrs}=Res, Key, Val) when is_binary(Key); is_atom(Key) ->
    case orddict:is_key(Key, Attrs) of
        true ->
            Attr = orddict:fetch(Key, Attrs),
            Res#occi_resource{attributes=orddict:store(Key, occi_attribute:set_value(Attr, Val), Attrs)};
        false ->
            {error, {undefined_attribute, Key}}
    end.

-spec update_attr_value(occi_resource(), term()) -> occi_resource().
update_attr_value(#occi_resource{attributes=Attrs}=Res, List) ->
    New_attr = orddict:merge(fun(_Key, Val, Val2) ->
                                     Val3 =occi_attribute:get_value(Val2),
                                     case Val3 of
                                         undefined -> Val;
                                         _ -> Val2
                                     end
                             end, Attrs, List),
    Res#occi_resource{attributes=New_attr}.

-spec get_attr(occi_resource(), occi_attr_key()) -> any().
get_attr(#occi_resource{id=Id}, 'occi.core.id') ->
    A = occi_attribute:core_id(),
    A#occi_attr{value=Id};
get_attr(#occi_resource{attributes=Attr}, Key) ->
    orddict:find(Key, Attr).

get_attr_value(#occi_resource{}=R, 'occi.core.id') ->
    get_id(R);
get_attr_value(#occi_resource{attributes=Attr}, Key) ->
    case orddict:find(Key, Attr) of
        {ok, #occi_attr{value=V}} -> V;
        _ -> throw({error, invalid_attribute})
    end.	    

-spec get_attributes(occi_resource()) -> [occi_attr()].
get_attributes(#occi_resource{attributes=Attrs}) ->
    orddict:fold(fun (_Key, Value, Acc) -> [Value|Acc] end, [], Attrs).

-spec add_link(occi_resource(), uri()) -> occi_resource().
add_link(#occi_resource{links=Links}=Res, #uri{}=Link) ->
    Res#occi_resource{links=sets:add_element(Link, Links)};

add_link(#occi_resource{links=Links}=Res, #occi_link{}=Link) ->
    Res#occi_resource{links=sets:add_element(Link, Links)}.

-spec get_links(occi_resource()) -> [uri()].
get_links(#occi_resource{links=Links}) ->
    sets:to_list(Links).

-spec get_links_size(occi_resource()) -> integer().
get_links_size(#occi_resource{links=Links}) ->
    sets:size(Links).

-spec reset(occi_resource()) -> occi_resource().
reset(#occi_resource{attributes=Attrs}=Res) ->
    Res#occi_resource{attributes=orddict:map(fun (_Key, Attr) ->
                                                      occi_attribute:reset(Attr)
                                             end, Attrs)}.

-spec add_prefix(occi_resource(), string()) -> occi_resource().
add_prefix(#occi_resource{links=Links}=Res, Prefix) ->
    Links2 = sets:fold(fun (#uri{}=U, Acc) ->
			       sets:add_element(occi_uri:add_prefix(U, Prefix), Acc);
			   (#occi_link{}=L, Acc) ->
                               sets:add_element(occi_link:add_prefix(L, Prefix), Acc)
                       end, sets:new(), Links),
    Res#occi_resource{links=Links2}.

-spec rm_prefix(occi_resource(), string()) -> occi_resource().
rm_prefix(#occi_resource{links=Links}=Res, Prefix) ->
    Links2 = sets:fold(fun (#uri{}=U, Acc) ->
                                sets:add_element(occi_uri:rm_prefix(U, Prefix), Acc);
                          (#occi_link{}=L, Acc) ->
                               sets:add_element(occi_link:rm_prefix(L, Prefix), Acc)
                       end, sets:new(), Links),
    Res#occi_resource{links=Links2}.


-spec has_category(occi_resource(), occi_cid()) -> true | false.
has_category(#occi_resource{cid=Cid}, Cid) ->
    true;
has_category(#occi_resource{mixins=undefined}, _) ->
    false;
has_category(#occi_resource{mixins=Mixins}, Cid) ->
    sets:is_element(Cid, Mixins).


-spec match_attr(occi_resource(), binary() | atom(), binary()) -> true | false.
match_attr(#occi_resource{attributes=Attr}, '_', Val) ->
    match_attr2(orddict:to_list(Attr), Val);

match_attr(#occi_resource{attributes=Attr}, Name, Val) ->
    case orddict:find(Name, Attr) of
        {ok, A} -> occi_attribute:match_value(A, Val);
        _ -> false
    end.	    

%%%
%%% Priv
%%%
match_attr2([], _) ->
    false;

match_attr2([{_, Attr} | Rest], Val) ->
    case occi_attribute:match_value(Attr, Val) of
	true -> true;
	false -> match_attr2(Rest, Val)
    end.
