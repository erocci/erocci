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
%%% @doc use 
%%% @end
%%% Created : 1 Aug 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_renderer_dbus).

-behaviour(occi_renderer).

-include("occi_dbus.hrl").
-include_lib("erocci_core/include/occi.hrl").
-include_lib("dbus/include/dbus.hrl").

-export([render/1,
	 render/2]).

-define(sig_resource, [variant, 
		       {struct, [string, string]}, 
		       {array, {struct, [string, string]}},
		       {dict, string, variant},
		       {array, variant}]
       ).

-define(sig_link, [variant, 
		   {struct, [string, string]}, 
		   {array, {struct, [string, string]}},
		   string,
		   string,
		   {struct, [string, string]},
		   {dict, string, variant}]
       ).

-define(sig_collection, [variant,
			 {array, string}]
       ).

-define(sig_mixin, [variant,
		    string]
       ).

%%%
%%% API
%%%
render(Obj) ->
    render(Obj, []).


render(#occi_action{id=Id, attributes=Attrs}, _) ->
    { render_cid(Id), render_attrs(Attrs) };


render(#occi_node{id=Id, objid=Objid, type=Type, owner=Owner, etag=Etag, data=Data}=Node, _) ->
    { render_uri(Id), 
      render_objid(Type, Objid),
      render_owner(Owner),
      render_etag(Etag),
      render_type(Node), 
      render_content(Type, Data) 
    }.


%%%
%%% Private
%%%
render_objid(_, '_') ->
    ?dbus_undefined;

render_objid(_, undefined) ->
    ?dbus_undefined;

render_objid(capabilities, #occi_cid{}=Objid) ->
    #dbus_variant{type={struct, [string, string]}, value=render_cid(Objid)};

render_objid(_, #uri{}=Objid) ->
    #dbus_variant{type=string, value=render_uri(Objid)};

render_objid(_, Objid) when is_integer(Objid) ->
    #dbus_variant{type=uint64, value=Objid}.


render_cid('_') ->
    {<<>>, <<>>};
	
render_cid(undefined) ->
    {<<>>, <<>>};
	
render_cid(#occi_cid{scheme=Scheme}=Cid) when is_atom(Scheme) ->
    render_cid(Cid#occi_cid{scheme=atom_to_binary(Scheme, utf8)});
	
render_cid(#occi_cid{term=Term}=Cid) when is_atom(Cid) ->
    render_cid(Cid#occi_cid{term=Term});

render_cid(#occi_cid{scheme=Scheme, term=Term}) ->
    {Scheme, Term}.


render_owner('_') ->
    ?dbus_undefined;

render_owner(undefined) ->
    ?dbus_undefined;

render_owner(Owner) when is_binary(Owner) ->
    #dbus_variant{type=string, value=Owner}.

render_etag('_') ->
    <<>>;

render_etag(undefined) ->
    <<>>;

render_etag(Etag) when is_binary(Etag) ->
    Etag.


render_mixins(Mixins) ->
    sets:fold(fun (Mixin, Acc) -> [ render_cid(Mixin) | Acc] end, [], Mixins).

render_attrs(Attrs) ->
    orddict:foldl(fun (Name, Attr, Acc) when is_atom(Name) ->
			  [{atom_to_binary(Name, utf8), render_attr_value(Attr)} | Acc];
		      (Name, Attr, Acc) ->
			  [{Name, render_attr_value(Attr)} | Acc]
		  end, [], Attrs).


render_attr_value(#occi_attr{type={?xmlschema_ns, anyURI}, value=#uri{}=Uri}) ->
    #dbus_variant{type=string, value=render_uri(Uri)};

render_attr_value(#occi_attr{type={?xmlschema_ns, string}, value=Val}) ->
    #dbus_variant{type=string, value=Val};

render_attr_value(#occi_attr{type={?xmlschema_ns, integer}, value=Val}) ->
    #dbus_variant{type=int64, value=Val};

render_attr_value(#occi_attr{type={?xmlschema_ns, float}, value=Val}) ->
    #dbus_variant{type=double, value=Val}.


render_type(#occi_node{type='_'})                                -> ?TYPE_UNDEFINED;
render_type(#occi_node{type=undefined})                          -> ?TYPE_UNDEFINED;
render_type(#occi_node{type=occi_collection, objid=#occi_cid{}}) -> ?TYPE_BOUNDED_COLL;
render_type(#occi_node{type=occi_collection, objid=#uri{}})      -> ?TYPE_UNBOUNDED_COLL;
render_type(#occi_node{type=occi_resource})                      -> ?TYPE_RESOURCE;
render_type(#occi_node{type=occi_link})                          -> ?TYPE_LINK;
render_type(#occi_node{type=capabilities, objid=#occi_cid{}})    -> ?TYPE_MIXIN.


render_content(_, '_') ->
    ?dbus_undefined;

render_content(_, undefined) ->
    ?dbus_undefined;

render_content(_, #occi_resource{id=Id, cid=Kind, mixins=Mixins, attributes=Attrs, links=Links}) ->
    Val = [ 
	    render_objid(occi_resource, Id),
	    render_cid(Kind),
	    render_mixins(Mixins),
	    render_attrs(Attrs),
	    render_resource_links(Links)
	  ],
    #dbus_variant{type=?sig_resource, value=Val};

render_content(_, #occi_link{id=Id, cid=Kind, mixins=Mixins, 
			     source=Src, target=Target, 
			     target_cid=TargetCid,
			     attributes=Attrs}) ->
    Val = [ 
	    render_objid(occi_link, Id),
	    render_cid(Kind),
	    render_mixins(Mixins),
	    render_uri(Src),
	    render_uri(Target),
	    render_cid(TargetCid),
	    render_attrs(Attrs)
	  ],
    #dbus_variant{type=?sig_link, value=Val};

render_content(Type, #occi_collection{id=Id, entities=Entities}) ->
    Val = ordsets:fold(fun (Uri, Acc) ->
			       [ render_uri(Uri) | Acc ]
		       end, [], Entities),
    #dbus_variant{type=?sig_collection, value={render_objid(Type, Id), Val}};

render_content(_, #occi_mixin{id=Id, location=Loc}) ->
    #dbus_variant{type=?sig_mixin, value={render_objid(capabilities, Id), render_uri(Loc)}}.


render_resource_links(Links) ->
    sets:fold(fun (#uri{}=Link, Acc) ->
		      [ #dbus_variant{type=string, value=render_uri(Link)} | Acc ];
		  (#occi_link{}=Link, Acc) ->
		      [ render_content(occi_link, Link) | Acc ]
	      end, Links).


render_uri('_') ->
    <<>>;

render_uri(undefined) ->
    <<>>;

render_uri(#uri{path=Path}) ->
    list_to_binary(Path).
