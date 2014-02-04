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
	 del_mixin/2,
	 set_attr_value/3,
	 add_attr_value/3,
	 get_attr/2,
	 get_attributes/1,
	 add_link/2,
	 get_links/1,
	 set_title/2,
	 get_title/1]).

-export([reset/1]).

%%%
%%% API
%%%
-spec new() -> occi_resource().
new() ->
    #occi_resource{attributes=orddict:new(), links=sets:new()}.

-spec new(occi_kind() | uri()) -> occi_resource().
new(#occi_kind{}=Kind) ->
    #occi_resource{cid=occi_kind:get_id(Kind), 
		   attributes=occi_kind:get_attributes(Kind),
		   links=sets:new()};
new(#uri{}=Id) ->
    #occi_resource{id=Id, attributes=orddict:new(), links=sets:new()}.

-spec get_id(occi_resource()) -> uri().
get_id(#occi_resource{id=Id}) ->
    Id.

-spec set_id(occi_resource(), uri() | string()) -> occi_resource().
set_id(#occi_resource{}=Res, Id) when is_list(Id) ->
    set_id(Res, occi_uri:parse(Id));
set_id(#occi_resource{}=Res, #uri{}=Id) ->
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

-spec get_title(occi_resource()) -> binary().
get_title(#occi_resource{title=Title}) ->
    Title.

-spec set_title(occi_resource(), binary()) -> occi_resource().
set_title(#occi_resource{}=Res, Title) when is_binary(Title) ->
    Res#occi_resource{title=Title}.

-spec get_mixins(occi_resource()) -> [occi_cid()].
get_mixins(#occi_resource{mixins=Mixins}) ->
    Mixins.

-spec add_mixin(occi_resource(), occi_mixin()) -> occi_resource().
add_mixin(#occi_resource{mixins=Mixins, attributes=Attrs}=Res, #occi_mixin{id=Cid}=Mixin) ->
    Attrs2 = orddict:merge(fun (_Key, _Val1, Val2) ->
				   Val2
			   end, Attrs, occi_mixin:get_attributes(Mixin)),
    Res#occi_resource{mixins=[Cid|Mixins], attributes=Attrs2}.

-spec del_mixin(occi_resource(), occi_mixin()) -> occi_resource().
del_mixin(#occi_resource{mixins=Mixins, attributes=Attrs}=Res, 
	  #occi_mixin{id=Cid, attributes=MixinAttrs}) ->
    Attrs2 = lists:foldl(fun (Key, Acc) ->
				 orddict:erase(Key, Acc)
			 end, Attrs, orddict:fetch_keys(MixinAttrs)),
    Mixins2 = lists:delete(Cid, Mixins),
    Res#occi_resource{mixins=Mixins2, attributes=Attrs2}.

-spec set_attr_value(occi_resource(), occi_attr_key(), any()) -> occi_resource().
set_attr_value(#occi_resource{}=Res, 'occi.core.title', Val) ->
    Res#occi_resource{title=Val};
set_attr_value(#occi_resource{}=Res, 'occi.core.id', Val) ->
    Res#occi_resource{id=Val};
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
add_link(#occi_resource{links=Links}=Res, #uri{}=Link) ->
    Res#occi_resource{links=sets:add_element(Link, Links)}.

-spec get_links(occi_resource()) -> [uri()].
get_links(#occi_resource{links=Links}) ->
    sets:to_list(Links).

-spec reset(occi_resource()) -> occi_resource().
reset(#occi_resource{attributes=Attrs}=Res) ->
    Res#occi_resource{attributes=orddict:map(fun (_Key, Attr) ->
						     occi_attribute:reset(Attr)
					     end, Attrs)}.
