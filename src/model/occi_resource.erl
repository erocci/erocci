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

-export([new/0,
	 new/1,
	 get_id/1,
	 set_id/2,
	 get_cid/1,
	 set_cid/2,
	 get_mixins/1,
	 add_mixin/2,
	 set_attr_value/3,
	 add_attr_value/3,
	 get_attr/2,
	 get_attributes/1,
	 add_link/2,
	 get_links/1]).

%%%
%%% API
%%%
-spec new() -> occi_resource().
new() ->
    #occi_resource{attributes=orddict:new()}.

-spec new(occi_kind()) -> occi_resource().
new(#occi_kind{}=Kind) ->
    #occi_resource{cid=occi_kind:get_id(Kind), 
		   attributes=occi_kind:get_attributes(Kind)}.

-spec get_id(occi_resource()) -> uri().
get_id(#occi_resource{id=Id}) ->
    Id.

-spec set_id(occi_resource(), uri()) -> occi_resource().
set_id(#occi_resource{}=Res, Id) ->
    Res#occi_resource{id=Id}.

-spec get_cid(occi_resource()) -> occi_cid().
get_cid(#occi_resource{cid=Cid}) ->
    Cid.

-spec set_cid(occi_resource(), occi_cid()) -> occi_resource().
set_cid(#occi_resource{attributes=Attrs}=Res, #occi_cid{class=kind}=Cid) ->
    case occi_category_mgr:get(Cid) of
	undefined ->
	    throw({unknown_category, Cid});
	Kind ->
	    Attrs2 = orddict:merge(fun (_Key, _Val1, Val2) ->
					   Val2
				   end, Attrs, occi_kind:get_attributes(Kind)),
	    Res#occi_resource{cid=Cid, attributes=Attrs2}
    end.

-spec get_mixins(occi_resource()) -> [occi_cid()].
get_mixins(#occi_resource{mixins=Mixins}) ->
    Mixins.

-spec add_mixin(occi_resource(), occi_cid()) -> occi_resource().
add_mixin(#occi_resource{mixins=Mixins, attributes=Attrs}=Res, #occi_cid{class=mixin}=Cid) ->
    Mixin = occi_category_mgr:get(Cid),
    Attrs2 = orddict:merge(fun (_Key, _Val1, Val2) ->
				   Val2
			   end, Attrs, occi_mixin:get_attributes(Mixin)),
    Res#occi_resource{mixins=[Cid|Mixins], attributes=Attrs2}.

-spec set_attr_value(occi_resource(), occi_attr_key(), any()) -> occi_resource().
set_attr_value(#occi_resource{}=Res, Key, Val) when is_list(Key) ->
    set_attr_value(Res, list_to_atom(Key), Val);
set_attr_value(#occi_resource{attributes=Attrs}=Res, Key, Val) when is_atom(Key) ->
    case orddict:is_key(Key, Attrs) of
	true ->
	    Attr = orddict:fetch(Key, Attrs),
	    Res#occi_resource{attributes=orddict:store(Key, occi_attribute:set_value(Attr, Val), Attrs)};
	false ->
	    {error, {undefined_attribute, Key}}
    end.

-spec add_attr_value(occi_resource(), occi_attr_key(), any()) -> occi_resource().
add_attr_value(#occi_resource{}=Res, Key, Val) when is_list(Key) ->
    add_attr_value(Res, list_to_atom(Key), Val);
add_attr_value(#occi_resource{attributes=Attrs}=Res, Key, Val) when is_atom(Key) ->
    case orddict:is_key(Key, Attrs) of
	true ->
	    Attr = orddict:fetch(Key, Attrs),
	    Res#occi_resource{attributes=orddict:store(Key, occi_attribute:add_value(Attr, Val))};
	false ->
	    {error, {undefined_attribute, Key}}
    end.

-spec get_attr(occi_resource(), occi_attr_key()) -> any().
get_attr(#occi_resource{attributes=Attr}, Key) ->
    orddict:find(Key, Attr).

-spec get_attributes(occi_resource()) -> [occi_attr()].
get_attributes(#occi_resource{attributes=Attrs}) ->
    orddict:fold(fun (_Key, Value, Acc) -> [Value|Acc] end, [], Attrs).

-spec add_link(occi_resource(), uri()) -> occi_resource().
add_link(Res, Link) when is_list(Link) ->
    add_link(Res, list_to_binary(Link));
add_link(#occi_resource{links=Links}=Res, Link) ->
    Res#occi_resource{links=[Link|Links]}.

-spec get_links(occi_resource()) -> [uri()].
get_links(#occi_resource{links=Links}) ->
    Links.
