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
%%% Created : 1 Aug 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_parser_dbus).

-include("occi_dbus.hrl").
-include("occi.hrl").
-include_lib("dbus/include/dbus.hrl").

%% API
-export([parse/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse({Id, _, _, Etag, ?TYPE_CAPABILITIES, Content}) ->
    #occi_node{id=occi_uri:parse(Id),
	       objid=undefined,
	       owner=nobody,
	       etag=Etag,
	       type=capabilities,
	       data=parse_capabilities(Content)};

parse({Id, Objid, Owner, Etag, ?TYPE_BOUNDED_COLL, Content}) ->
    Cid = parse_objid(?TYPE_BOUNDED_COLL, Objid),
    #occi_node{id=occi_uri:parse(Id),
	       objid=Cid,
	       owner=parse_owner(Owner),
	       etag=Etag,
	       type=occi_collection,
	       data=parse_collection(Cid, Content)};

parse({Id, Objid, Owner, Etag, ?TYPE_UNBOUNDED_COLL, Content}) ->
    CollId = parse_objid(?TYPE_UNBOUNDED_COLL, Objid),
    #occi_node{id=occi_uri:parse(Id),
	       objid=CollId,
	       owner=parse_owner(Owner),
	       etag=Etag,
	       type=occi_collection,
	       data=parse_collection(CollId, Content)};

parse({Id, Objid, Owner, Etag, ?TYPE_RESOURCE, Content}) ->
    ResId = parse_objid(?TYPE_RESOURCE, Objid),
    #occi_node{id=occi_uri:parse(Id),
	       objid=ResId,
	       owner=parse_owner(Owner),
	       etag=Etag,
	       type=occi_resource,
	       data=parse_resource(ResId, Content)};

parse({Id, Objid, Owner, Etag, ?TYPE_LINK, Content}) ->
    LinkId = parse_objid(?TYPE_LINK, Objid),
    #occi_node{id=occi_uri:parse(Id),
	       objid=LinkId,
	       owner=parse_owner(Owner),
	       etag=Etag,
	       type=occi_resource,
	       data=parse_link(LinkId, Content)};

parse({Id, Objid, Owner, Etag, ?TYPE_MIXIN, Content}) ->
    Cid = parse_objid(?TYPE_MIXIN, Objid),
    #occi_node{id=occi_uri:parse(Id),
	       objid=Cid,
	       owner=parse_owner(Owner),
	       etag=Etag,
	       type=capabilities,
	       data=parse_mixin(Cid, Content)}.


%%%
%%% Priv
%%%
parse_objid(_, #dbus_variant{value=false}) ->
    undefined;

parse_objid(?TYPE_BOUNDED_COLL, Var) ->
    parse_cid(Var);

parse_objid(?TYPE_MIXIN, Var) ->
    parse_cid(Var);

parse_objid(_, #dbus_variant{type=string, value=Val}) ->
    occi_uri:parse(Val);

parse_objid(_, #dbus_variant{value=Val}) when is_integer(Val) ->
    Val.


parse_cid(#dbus_variant{type={struct, [string, string]}, value={Scheme, Term}}) ->
    #occi_cid{scheme=?scheme_to_atom(Scheme), term=?term_to_atom(Term)}.


parse_uri(#dbus_variant{type=string, value=Val}) ->
    occi_uri:parse(Val).


parse_owner(?dbus_undefined) ->
    undefined;

parse_owner(#dbus_variant{type=string, value=Val}) ->
    Val.


parse_collection(_, #dbus_variant{value=false}) ->
    undefined;

parse_collection(Id, #dbus_variant{value={_, Uris}}) ->
    Entities = lists:foldl(fun (S, Acc) ->
				   [ occi_uri:parse(S) | Acc ]
			   end, [], Uris),
    occi_collection:new(Id, Entities).


parse_resource(_, #dbus_variant{value=false}) ->
    undefined;

parse_resource(Id, #dbus_variant{value={_, {Scheme, Term}, Mixins, Attrs, Links}}) ->
    Kind = get_kind(Scheme, Term),
    Mixins = parse_mixins(Mixins),
    Attributes = parse_attributes(Attrs),
    Res = occi_resource:new(Id, Kind, Mixins, Attributes),
    lists:foldl(fun (#dbus_variant{type=string, value=S}, Acc) ->
			occi_resource:add_link(Acc, occi_uri:parse(S));
		    (Other, Acc) ->
			occi_resource:add_link(Acc, parse_resource_link(Other))
		end, Res, Links).


parse_resource_link(#dbus_variant{value={Id, _ ,_ ,_ ,_ ,_ ,_}}=Link) ->
    parse_link(parse_objid(?TYPE_LINK, Id), Link).


parse_link(_, #dbus_variant{value=false}) ->
    undefined;

parse_link(Id, #dbus_variant{value={_, {Scheme, Term}, Mixins, Src, Target, TargetCid, Attrs}}) ->
    Kind = get_kind(Scheme, Term),
    Mixins = parse_mixins(Mixins),
    Attributes = parse_attributes(Attrs),
    Target = parse_uri(Target),
    occi_link:set_target_cid(
      occi_link:set_source(
	occi_link:new(Id, Kind, Mixins, Attributes, Target), Src), parse_cid(TargetCid)).


parse_capabilities(#dbus_variant{value=false}) ->
    undefined;

parse_capabilities(#dbus_variant{value=Mixins}) ->
    M = lists:map(fun ({{Scheme, Term}, Uri}) ->
			  occi_mixin:new(#occi_cid{scheme=Scheme, term=Term, class=mixin}, 
					 occi_uri:parse(Uri))
		  end, Mixins),
    {[], M, []}.


parse_mixin(_, #dbus_variant{value=false}) ->
    undefined;

parse_mixin(Id, #dbus_variant{value={_, Location}}) ->
    #occi_mixin{id=Id, location=occi_uri:parse(Location)}.


parse_mixins(Mixins) ->
    lists:map(fun ({Scheme, Term}) ->
		      get_mixin(Scheme, Term)
	      end, Mixins).


parse_attributes(Attrs) ->
    lists:map(fun ({Name, #dbus_variant{value=Val}}) ->
		      {?attr_to_atom(Name), Val}
	      end, Attrs).


get_kind(Scheme, Term) ->
    Cid = #occi_cid{scheme=?scheme_to_atom(Scheme), term=?term_to_atom(Term), class=kind},
    case occi_category_mgr:get(Cid) of
	{ok, Kind} -> Kind;
	_ -> throw({error, {invalid_kind, Scheme, Term}})
    end.


get_mixin(Scheme, Term) ->
    Cid = #occi_cid{scheme=?scheme_to_atom(Scheme), term=?term_to_atom(Term), class=mixin},
    case occi_category_mgr:get(Cid) of
	{ok, Mixin} -> Mixin;
	_ -> throw({error, {invalid_mixin, Scheme, Term}})
    end.
