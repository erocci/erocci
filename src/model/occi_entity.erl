%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
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
%%% Created : 8 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_entity).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-define(scheme_core, 'http://schemas.ogf.org/occi/core#').
-define(cid_resource, #occi_cid{scheme=?scheme_core, term=resource}).
-define(cid_link, #occi_cid{scheme=?scheme_core, term=link}).

-export([new/1,
	 new/2,
	 set_id/2,
	 add_mixin/2,
	 set_attr_value/3]).
-export([merge_attrs/2,
	rm_attrs/2]).

-spec new(occi_kind()) -> occi_resource() | occi_link().
new(#occi_kind{}=Kind) ->
    case occi_kind:get_parent(Kind) of
	?cid_resource ->
	    occi_resource:new(Kind);
	?cid_link ->
	    occi_link:new(Kind)
    end.

-spec new(uri(), occi_kind()) -> occi_resource() | occi_link().
new(Id, #occi_kind{}=Kind) ->
    case occi_kind:get_parent(Kind) of
	?cid_resource ->
	    occi_resource:new(Id, Kind);
	?cid_link ->
	    occi_link:new(Id, Kind)
    end.

-spec set_id(occi_resource() | occi_link(), uri()) -> occi_resource() | occi_link().
set_id(#occi_resource{}=R, Id) ->
    occi_resource:set_attr_value(R, 'occi.core.id', Id);
set_id(#occi_link{}=L, Id) ->
    occi_link:set_attr_value(L, 'occi.core.id', Id).

-spec add_mixin(occi_resource() | occi_link(), occi_mixin()) -> occi_resource() | occi_link().
add_mixin(#occi_resource{}=Res, Mixin) ->
    occi_resource:add_mixin(Res, Mixin);
add_mixin(#occi_link{}=Link, Mixin) ->
    occi_link:add_mixin(Link, Mixin).

-spec set_attr_value(occi_resource() | occi_link(), atom(), term()) -> occi_resource() | occi_link().
set_attr_value(#occi_resource{}=Res, Name, Value) ->
    occi_resource:set_attr_value(Res, Name, Value);
set_attr_value(#occi_link{}=Link, Name, Value) ->
    occi_link:set_attr_value(Link, Name, Value).

merge_attrs(#occi_kind{}=Kind, Attrs) ->
    orddict:merge(fun (_Key, _Val1, Val2) ->
			  Val2
		  end, Attrs, occi_kind:get_attributes(Kind));
merge_attrs(#occi_mixin{}=Mixin, Attrs) ->
    orddict:merge(fun (_Key, _Val1, Val2) ->
			  Val2
		  end, Attrs, occi_mixin:get_attributes(Mixin)).

rm_attrs(#occi_mixin{attributes=MixinAttrs}, Attrs) ->
    lists:foldl(fun (Key, Acc) ->
			orddict:erase(Key, Acc)
		end, Attrs, orddict:fetch_keys(MixinAttrs)).
