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
-module(occi_link).

-include("occi.hrl").

-export([new/0,
         new/1,
         new/2,
         new/5,
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
         get_target/1,
         set_target/2,
         get_target_cid/1,
         set_target_cid/2,
         get_source/1,
         set_source/2,
         add_prefix/2,
         rm_prefix/2,
         update_attr_value/2,
	 has_category/2,
	 match_attr/3]).

-export([reset/1]).

-define(CORE_ATTRS, [{'occi.core.title', occi_attribute:core_title()}]).

%%%
%%% API
%%%
-spec new() -> occi_link().
new() ->
    #occi_link{attributes=orddict:from_list(?CORE_ATTRS)}.

-spec new(Id :: occi_objid(), Kind :: occi_kind()) -> occi_link().
new(Id, #occi_kind{}=Kind) ->
    Attrs = [orddict:to_list(occi_kind:get_attributes(Kind)),
	     ?CORE_ATTRS],
    #occi_link{id=Id, cid=occi_kind:get_id(Kind), 
               attributes=orddict:from_list(lists:flatten(Attrs))}.

-spec new(occi_kind() | uri()) -> occi_link().
new(#occi_kind{}=Kind) ->
    Attrs = [orddict:to_list(occi_kind:get_attributes(Kind)),
	     ?CORE_ATTRS],
    #occi_link{cid=occi_kind:get_id(Kind), 
               attributes=orddict:from_list(lists:flatten(Attrs))};

new(Id) ->
    #occi_link{id=Id, attributes=orddict:from_list(?CORE_ATTRS)}.

-spec new(occi_objid(), occi_kind(), [occi_mixin()], [{atom(), term}], uri()) -> occi_link().
new(Id, #occi_kind{}=Kind, Mixins, Attributes, Target) ->
    Attrs = [?CORE_ATTRS,
	     orddict:to_list(occi_kind:get_attributes(Kind)),
	     lists:map(fun (Mixin) ->
			       orddict:to_list(occi_kind:get_attributes(Mixin))
		       end, Mixins)],
    L = #occi_link{id=Id,
                   cid=occi_kind:get_id(Kind), 
                   attributes=orddict:from_list(lists:flatten(Attrs)),
                   target=Target},
    lists:foldl(fun ({Key, Value}, Acc) ->
                         occi_link:set_attr_value(Acc, Key, Value)
                end, L, Attributes).

-spec get_id(occi_link()) -> uri().
get_id(#occi_link{id=Id}) ->
    Id.

-spec set_id(occi_link(), occi_objid()) -> occi_link().
set_id(#occi_link{}=L, Id) ->
    L#occi_link{id=Id}.

-spec get_source(occi_link()) -> uri().
get_source(#occi_link{source=Src}) ->
    Src.

-spec set_source(occi_link(), uri()) -> occi_link().
set_source(#occi_link{}=Link, #uri{}=Uri) ->
    Link#occi_link{source=Uri};
set_source(#occi_link{}=Link, Uri) ->
    Link#occi_link{source=occi_uri:parse(Uri)}.

-spec get_target(occi_link()) -> uri().
get_target(#occi_link{target=Target}) ->
    Target.

-spec set_target(occi_link(), uri()) -> occi_link().
set_target(#occi_link{}=Link, #uri{}=Uri) ->
    Link#occi_link{target=Uri};
set_target(#occi_link{}=Link, Uri) ->
    Link#occi_link{target=occi_uri:parse(Uri)}.

-spec get_target_cid(occi_link()) -> occi_cid().
get_target_cid(#occi_link{target_cid=C}) ->
    C.

-spec set_target_cid(occi_link(), occi_cid()) -> occi_cid().
set_target_cid(#occi_link{}=L, #occi_cid{}=C) ->
    L#occi_link{target_cid=C}.

-spec get_cid(occi_link()) -> occi_cid().
get_cid(#occi_link{cid=Cid}) ->
    Cid.

-spec set_cid(occi_link(), occi_kind()) -> occi_link().
set_cid(#occi_link{attributes=Attrs}=Res, #occi_kind{id=Cid}=Kind) ->
    Res#occi_link{cid=Cid, attributes=occi_entity:merge_attrs(Kind, Attrs)}.

-spec get_mixins(occi_link()) -> term(). % return set()
get_mixins(#occi_link{mixins=undefined}) ->
    sets:new();
get_mixins(#occi_link{mixins=Mixins}) ->
    Mixins.

-spec add_mixin(occi_link(), occi_mixin()) -> occi_link().
add_mixin(#occi_link{mixins=undefined}=Link, Mixin) ->
    add_mixin(Link#occi_link{mixins=sets:new()}, Mixin);
add_mixin(#occi_link{mixins=Mixins, attributes=Attrs}=Res, #occi_mixin{id=Cid}=Mixin) ->
    Res#occi_link{mixins=sets:add_element(Cid, Mixins), 
                  attributes=occi_entity:merge_attrs(Mixin, Attrs)}.

-spec del_mixin(occi_link(), occi_mixin()) -> occi_link().
del_mixin(#occi_link{mixins=undefined}=Link, _) ->
    Link;
del_mixin(#occi_link{mixins=Mixins, attributes=Attrs}=Res, #occi_mixin{id=Cid}=Mixin) ->
    Res#occi_link{mixins=lists:delete(Cid, Mixins), 
                  attributes=occi_entity:rm_attrs(Mixin, Attrs)}.

-spec set_attr_value(occi_link(), occi_attr_key(), any()) -> occi_link().
set_attr_value(#occi_link{}=Link, 'occi.core.source', Val) ->
    set_source(Link, Val);
set_attr_value(#occi_link{}=Link, 'occi.core.target', Val) ->
    set_target(Link, Val);
set_attr_value(#occi_link{attributes=Attrs}=Link, Key, Val) when is_binary(Key); is_atom(Key) ->
    case orddict:is_key(Key, Attrs) of
        true ->
            Attr = orddict:fetch(Key, Attrs),
            Link#occi_link{attributes=orddict:store(Key, occi_attribute:set_value(Attr, Val), Attrs)};
        false ->
            {error, {undefined_attribute, Key}}
    end.

-spec update_attr_value(occi_resource(), term()) -> occi_resource().
update_attr_value(#occi_link{attributes=Attrs}=Link, List) ->
    New_attr = orddict:merge(fun(_Key, Val, Val2) ->
                                     Val3 =occi_attribute:get_value(Val2),
                                     case Val3 of
                                         undefined -> Val;
                                         _ -> Val2
                                     end
                             end, Attrs, List),
    Link#occi_link{attributes=New_attr}.

-spec get_attr(occi_link(), occi_attr_key()) -> any().
get_attr(#occi_link{id=Val}, 'occi.core.id') ->
    A = occi_attribute:core_id(),
    A#occi_attr{id=Val};
get_attr(#occi_link{source=Val}, 'occi.core.source') ->
    A = occi_attribute:core_src(),
    A#occi_attr{value=Val};
get_attr(#occi_link{target=Val}, 'occi.core.target') ->
    A = occi_attribute:core_target(),
    A#occi_attr{value=Val};
get_attr(#occi_link{attributes=Attr}, Key) ->
    orddict:find(Key, Attr).

get_attr_value(#occi_link{attributes=Attr}, Key) ->
    case orddict:find(Key, Attr) of
        {ok, #occi_attr{value=V}} -> V;
        _ -> throw({error, invalid_attribute})
    end.	    

-spec get_attributes(occi_link()) -> [occi_attr()].
get_attributes(#occi_link{attributes=Attrs}) ->
    orddict:fold(fun (_Key, Value, Acc) -> [Value|Acc] end, [], Attrs).

-spec reset(occi_link()) -> occi_link().
reset(#occi_link{attributes=Attrs}=Link) ->
    Link#occi_link{attributes=orddict:map(fun (_Key, Attr) ->
                                                   occi_attribute:reset(Attr)
                                          end, Attrs)}.

-spec add_prefix(occi_link(), string()) -> occi_link().
add_prefix(#occi_link{source=Src, target=Target}=Link, Prefix) ->
    Link#occi_link{id=case Link#occi_link.id of
			  #uri{}=Uri -> occi_uri:add_prefix(Uri, Prefix);
			  Else -> Else
		      end,
		   source=occi_uri:add_prefix(Src, Prefix),
		   target=occi_uri:add_prefix(Target, Prefix)}.

-spec rm_prefix(occi_link(), string()) -> occi_link().
rm_prefix(#occi_link{source=Src, target=Target}=Link, Prefix) ->
    Link#occi_link{id=case Link#occi_link.id of
			  #uri{}=Uri -> occi_uri:rm_prefix(Uri, Prefix);
			  Else -> Else
		      end,
		   source=occi_uri:rm_prefix(Src, Prefix),
		   target=occi_uri:rm_prefix(Target, Prefix)}.


-spec has_category(occi_link(), occi_cid()) -> true | false.
has_category(#occi_link{cid=Cid}, Cid) ->
    true;
has_category(#occi_link{mixins=undefined}, _) ->
    false;
has_category(#occi_link{mixins=Mixins}, Cid) ->
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
