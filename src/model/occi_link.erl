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
	 get_title/1,
	 set_title/2,
	 get_target/1,
	 set_target/2,
	 get_source/1,
	 set_source/2]).

-export([reset/1]).

%%%
%%% API
%%%
-spec new() -> occi_link().
new() ->
    #occi_link{attributes=orddict:new()}.


-spec new(occi_kind() | uri()) -> occi_link().
new(#occi_kind{}=Kind) ->
    #occi_link{cid=occi_kind:get_id(Kind), 
	       attributes=occi_kind:get_attributes(Kind)};
new(#uri{}=Uri) ->
    #occi_link{id=Uri, attributes=orddict:new()}.

-spec get_id(occi_link()) -> uri().
get_id(#occi_link{id=Id}) ->
    Id.

-spec set_id(occi_link(), uri() | string()) -> occi_link().
set_id(#occi_link{}=Res, Id) when is_list(Id) ->
    set_id(Res, occi_uri:parse(Id));
set_id(#occi_link{}=Res, #uri{}=Id) ->
    Res#occi_link{id=Id}.

-spec get_cid(occi_link()) -> occi_cid().
get_cid(#occi_link{cid=Cid}) ->
    Cid.

-spec set_cid(occi_link(), occi_kind()) -> occi_link().
set_cid(#occi_link{attributes=Attrs}=Res, #occi_kind{id=Cid}=Kind) ->
    Attrs2 = orddict:merge(fun (_Key, _Val1, Val2) ->
				   Val2
			   end, Attrs, occi_kind:get_attributes(Kind)),
    Res#occi_link{cid=Cid, attributes=Attrs2}.

-spec get_title(occi_link()) -> binary().
get_title(#occi_link{title=Title}) ->
    Title.

-spec set_title(occi_link(), binary()) -> occi_link().
set_title(#occi_link{}=Res, Title) when is_binary(Title) ->
    Res#occi_link{title=Title}.

-spec get_mixins(occi_link()) -> set().
get_mixins(#occi_link{mixins=undefined}) ->
    sets:new();
get_mixins(#occi_link{mixins=Mixins}) ->
    Mixins.

-spec add_mixin(occi_link(), occi_mixin()) -> occi_link().
add_mixin(#occi_link{mixins=undefined}=Link, Mixin) ->
    add_mixin(Link#occi_link{mixins=sets:new()}, Mixin);
add_mixin(#occi_link{mixins=Mixins, attributes=Attrs}=Res, #occi_mixin{id=Cid}=Mixin) ->
    Attrs2 = orddict:merge(fun (_Key, _Val1, Val2) ->
				   Val2
			   end, Attrs, occi_mixin:get_attributes(Mixin)),
    Res#occi_link{mixins=sets:add_element(Cid, Mixins), attributes=Attrs2}.

-spec del_mixin(occi_link(), occi_mixin()) -> occi_link().
del_mixin(#occi_link{mixins=undefined}=Link, _) ->
    Link;
del_mixin(#occi_link{mixins=Mixins, attributes=Attrs}=Res, 
	  #occi_mixin{id=Cid, attributes=MixinAttrs}) ->
    Attrs2 = lists:foldl(fun (Key, Acc) ->
				 orddict:erase(Key, Acc)
			 end, Attrs, orddict:fetch_keys(MixinAttrs)),
    Mixins2 = lists:delete(Cid, Mixins),
    Res#occi_link{mixins=Mixins2, attributes=Attrs2}.

-spec set_attr_value(occi_link(), occi_attr_key(), any()) -> occi_link().
set_attr_value(#occi_link{}=Link, 'occi.core.title', Val) ->
    Link#occi_link{title=Val};
set_attr_value(#occi_link{}=Link, 'occi.core.id', #uri{}=Val) ->
    Link#occi_link{id=Val};
set_attr_value(#occi_link{}=Link, 'occi.core.target', #uri{}=Val) ->
    set_target(Link, Val);
set_attr_value(#occi_link{}=Link, 'occi.core.source', #uri{}=Val) ->
    set_source(Link, Val);
set_attr_value(#occi_link{}=Link, Key, Val) when is_list(Key) ->
    set_attr_value(Link, list_to_atom(Key), Val);
set_attr_value(#occi_link{attributes=Attrs}=Link, Key, Val) when is_atom(Key) ->
    case orddict:is_key(Key, Attrs) of
	true ->
	    Attr = orddict:fetch(Key, Attrs),
	    Link#occi_link{attributes=orddict:store(Key, occi_attribute:set_value(Attr, Val), Attrs)};
	false ->
	    {error, {undefined_attribute, Key}}
    end.

-spec add_attr_value(occi_link(), occi_attr_key(), any()) -> occi_link().
add_attr_value(#occi_link{}=Link, Key, Val) when is_list(Key) ->
    add_attr_value(Link, list_to_atom(Key), Val);
add_attr_value(#occi_link{attributes=Attrs}=Link, Key, Val) when is_atom(Key) ->
    case orddict:is_key(Key, Attrs) of
	true ->
	    Attr = orddict:fetch(Key, Attrs),
	    Link#occi_link{attributes=orddict:store(Key, occi_attribute:add_value(Attr, Val))};
	false ->
	    {error, {undefined_attribute, Key}}
    end.

-spec get_attr(occi_link(), occi_attr_key()) -> any().
get_attr(#occi_link{attributes=Attr}, Key) ->
    orddict:find(Key, Attr).

-spec get_attributes(occi_link()) -> [occi_attr()].
get_attributes(#occi_link{attributes=Attrs}) ->
    orddict:fold(fun (_Key, Value, Acc) -> [Value|Acc] end, [], Attrs).

-spec get_source(occi_link()) -> uri().
get_source(#occi_link{source=Uri}) ->
    Uri.

-spec set_source(occi_link(), uri()) -> occi_link().
set_source(#occi_link{}=Link, #uri{}=Uri) ->
    Link#occi_link{source=Uri}.

-spec get_target(occi_link()) -> uri().
get_target(#occi_link{target=Uri}) ->
    Uri.

-spec set_target(occi_link(), uri()) -> occi_link().
set_target(#occi_link{}=Link, #uri{}=Uri) ->
    Link#occi_link{target=Uri}.

-spec reset(occi_link()) -> occi_link().
reset(#occi_link{attributes=Attrs}=Link) ->
    Link#occi_link{attributes=orddict:map(fun (_Key, Attr) ->
						  occi_attribute:reset(Attr)
					  end, Attrs)}.
