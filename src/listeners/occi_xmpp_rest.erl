%%%-------------------------------------------------------------------
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
%%% Created : 31 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_xmpp_rest).
-compile({parse_transform, lager_transform}).

-include("occi.hrl").
-include("occi_xml.hrl").
-include("occi_xmpp.hrl").
-include_lib("erim/include/exmpp.hrl").

-export([init/2,
	 allowed_method/2,
	 resource_exists/2,
	 is_conflict/2,
	 delete_resource/2,
	 get_resource/2,
	 accept_resource/2]).

-record(state, {}).

-spec init(xmlel(), any()) -> {ok, xmlel(), any()}.
init(Req, _S) ->
    {ok, Req, #state{}}.

allowed_method(#occi_iq{type=occi_query}=Req, State) ->
    {['get', update, delete], Req, State};

allowed_method(#occi_iq{type=occi_collection}=Req, State) ->
    {['get', update, delete], Req, State};

allowed_method(Req, State) ->
    {['get', save, update, delete], Req, State}.

resource_exists(#occi_iq{type=occi_query}=Req, State) ->
    {true, Req, State};

resource_exists(#occi_iq{node=Node}=Req, State) ->
    case occi_store:find(Node) of
	{ok, []} ->
	    {false, Req, State};
	{ok, [Node2]} ->
	    lager:debug("Resource: ~p~n", [lager:pr(Node2, ?MODULE)]),
	    {true, Req#occi_iq{node=Node2}, State}
    end.

is_conflict(#occi_iq{type=occi_query}=Req, State) ->
    {true, Req, State};
is_conflict(#occi_iq{type=occi_collection}=Req, State) ->
    {false, Req, State};
is_conflict(#occi_iq{type=occi_entity, node=#occi_node{type=Type}}=Req, State) 
  when Type =:= occi_resource;
       Type =:= occi_link;
       Type =:= undefined ->
    {false, Req, State};
is_conflict(Req, State) ->
    {true, Req, State}.

delete_resource(#occi_iq{type=occi_query, node=Node}=Req, State) ->
    case occi_store:find(Node) of
	{ok, []} ->
	    Req2 = occi_iq:error(Req, 'item-not-found'),
	    {halt, Req2, State};
	{ok, [#occi_mixin{user=false}]} ->
	    Req2 = occi_iq:error(Req, 'forbidden'),
	    {halt, Req2, State};
	{ok, [#occi_mixin{}=Mixin]} ->
	    case occi_store:delete(Mixin) of
		{error, Err} ->
		    lager:debug("Error deleting user mixin: ~p~n", [Err]),
		    {false, Req, State};
		ok ->
		    {true, Req, State}
	    end
    end;
	    
delete_resource(#occi_iq{node=Node}=Req, State) ->
    case occi_store:delete(Node) of
	{error, Reason} ->
	    lager:error("Error deleting node: ~p~n", [Reason]),
	    {false, Req, State};
	ok ->
	    {true, Req, State}
    end.

get_resource(#occi_iq{node=Node}=Req, State) ->
    case occi_store:load(Node) of
	{ok, Node2} ->
	    {occi_renderer_xml:to_xmlel(Node2), Req, State};
	{error, Err} ->
	    lager:error("Internal error: ~p~n", [Err]),
	    {halt, Req, State}
    end.

accept_resource(#occi_iq{type=occi_query, raw=Raw}=Req, State) ->
    case exmpp_xml:get_child_elements(occi_iq:get_payload(Raw)) of
	[] -> 
	    lager:debug("Bad request: ~p~n", [lager:pr(Raw, ?MODULE)]),
	    Req2 = occi_iq:error(Req, 'bad-request'),
	    {false, Req2, State};
	[El|_] ->
	    case occi_parser_xml:parse_full(El) of
		{ok, #occi_request{mixins=[#occi_mixin{}=Mixin]}} ->
		    case occi_store:save(Mixin) of
			ok ->
			    {true, Req, State};
			{error, Err} ->
			    lager:error("Internal error: ~p~n", [Err]),
		    {halt, Req, State}
		    end;
		Other ->
		    lager:debug("Bad request: ~p~n", [Other]),
		    Req2 = occi_iq:error(Req, 'bad-request'),
		    {false, Req2, State}
	    end
    end;

accept_resource(#occi_iq{type=occi_entity, node=Node, raw=Raw}=Req, State) ->
    case exmpp_xml:get_child_elements(occi_iq:get_payload(Raw)) of
	[] ->
	    lager:debug("Bad request: ~p~n", [lager:pr(Raw, ?MODULE)]),
	    Req2 = occi_iq:error(Req, 'bad-request'),
	    {false, Req2, State};
	[El|_] ->
	    case occi_parser_xml:parse_full(El) of
		{error, {parse_error, Err}} ->
		    lager:error("Error processing request: ~p~n", [Err]),
		    {false, Req, State};
		{error, Err} ->
		    lager:error("Internal error: ~p~n", [Err]),
		    Req2 = occi_iq:error(Req, 'service-unavailable'),
		    {halt, Req2, State};
		{ok, #occi_request{entities=[#occi_resource{}=Res]}} ->
		    Node2 = create_resource_node(Node, Res),
		    case occi_store:save(Node2) of
			ok ->
			    Req2 = occi_iq:set_node_attr(Req, occi_node:get_objid(Node2)),
			    {true, Req2, State};
			{error, Reason} ->
			    lager:error("Error creating resource: ~p~n", [Reason]),
			    Req2 = occi_iq:error(Req, 'service-unavailable'),
			    {halt, Req2, State}
		    end;
		{ok, #occi_request{entities=[#occi_link{}=Link]}} ->
		    Node2 = create_link_node(Node, Link),
		    case occi_store:save(Node2) of
			ok ->
			    Req2 = occi_iq:set_node_attr(Req, occi_node:get_objid(Node2)),
			    {true, Req2, State};
			{error, Reason} ->
			    lager:error("Error creating link: ~p~n", [Reason]),
			    Req2 = occi_iq:error(Req, 'service-unavailable'),
			    {halt, Req2, State}
		    end;
		{ok, _} ->
		    lager:debug("Bad request: ~p~n", [lager:pr(Raw, ?MODULE)]),
		    Req2 = occi_iq:error(Req, 'bad-request'),
		    {false, Req2, State}   
	    end
    end.

%%%
%%% Priv
%%%
create_resource_node(#occi_node{objid=undefined}, #occi_resource{id=undefined, cid=Cid}=Res) ->
    {ok, [#occi_kind{location=#uri{path=Prefix}}]} = occi_store:find(Cid),
    Id = occi_config:gen_id(Prefix),
    occi_node:new(Id, occi_resource:set_id(Res, Id));
create_resource_node(#occi_node{objid=Uri}, #occi_resource{id=undefined}=Res) ->
    occi_node:new(Uri, occi_resource:set_id(Res, Uri));
create_resource_node(_Req, #occi_resource{}=Res) ->
    occi_node:new(occi_resource:get_id(Res), Res).

create_link_node(#occi_node{objid=undefined}, #occi_link{id=undefined, cid=Cid}=Link) ->
    {ok, [#occi_kind{location=#uri{path=Prefix}}]} = occi_store:find(Cid),
    Id = occi_config:gen_id(Prefix),
    occi_node:new(Id, occi_link:set_id(Link, Id));
create_link_node(#occi_node{objid=Uri}, #occi_link{id=undefined}=Link) ->
    occi_node:new(Uri, occi_link:set_id(Link, Uri));
create_link_node(_Req, #occi_link{}=Link) ->
    occi_node:new(occi_link:get_id(Link), Link).
