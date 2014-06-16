%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2013 Jean Parpaillon.
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
%% @doc Example webmachine_resource.

-module(occi_http_query).
-compile({parse_transform, lager_transform}).

-export([init/3, 
	 rest_init/2,
	 delete_resource/2,
	 allowed_methods/2,
	 is_authorized/2,
	 forbidden/2,
	 content_types_provided/2,
	 content_types_accepted/2
	]).
-export([to_plain/2, 
	 to_occi/2,
	 to_uri_list/2,
	 to_json/2,
	 to_xml/2]).
-export([from_plain/2,
	 from_occi/2,
	 from_json/2,
	 from_xml/2]).

-include("occi.hrl").
-include("occi_http.hrl").

-record(state, {op, node, ct, user}).

-record(content_type, {parser   :: atom(),
		       renderer :: atom(),
		       mimetype :: binary()}).

-define(ct_plain,    #content_type{parser=occi_parser_plain, renderer=occi_renderer_plain, 
				   mimetype="text/plain"}).
-define(ct_occi,     #content_type{parser=occi_parser_occi, renderer=occi_renderer_occi, 
				   mimetype="text/occi"}).
-define(ct_uri_list, #content_type{parser=occi_parser_uri_list, renderer=occi_renderer_uri_list, 
				   mimetype="text/uri-list"}).
-define(ct_json,     #content_type{parser=occi_parser_json, renderer=occi_renderer_json, 
				   mimetype="application/json"}).
-define(ct_xml,      #content_type{parser=occi_parser_xml, renderer=occi_renderer_xml, 
				   mimetype="application/xml"}).

init(_Transport, _Req, []) -> 
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    Op = occi_http_common:get_acl_op(Req),
    {ok, cowboy_req:set_resp_header(<<"server">>, ?SERVER_ID, Req), 
     #state{op=Op, node=#occi_node{type=capabilities}}}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"DELETE">>, <<"POST">>, <<"OPTIONS">>],
    << ", ", Allow/binary >> = << << ", ", M/binary >> || M <- Methods >>,
    {Methods, occi_http_common:set_cors(Req, Allow), State}.

is_authorized(Req, #state{op=Op, node=Node}=State) ->
    case occi_http_common:auth(Req) of
	{true, User} ->
	    {true, Req, State#state{user=User}};
	false ->
	    case occi_acl:check(Op, Node, anonymous) of
		allow ->
		    {true, Req, State#state{user=anonymous}};
		deny ->
		    {{false, occi_http_common:get_auth()}, Req, State}
	    end
    end.

forbidden(Req, #state{user=anonymous}=State) ->
    {false, Req, State};
forbidden(Req, #state{op=Op, user=User}=State) ->
    case occi_acl:check(Op, capabilities, User) of
	allow ->
	    {false, Req, State};
	deny ->
	    {true, Req, State}
    end.

content_types_provided(Req, State) ->
    {[
      {{<<"text">>,          <<"plain">>,     []}, to_plain},
      {{<<"text">>,          <<"occi">>,      []}, to_occi},
      {{<<"text">>,          <<"uri-list">>,  []}, to_uri_list},
      {{<<"application">>,   <<"occi+json">>, []}, to_json},
      {{<<"application">>,   <<"json">>,      []}, to_json},
      {{<<"application">>,   <<"xml">>,       []}, to_xml},
      {{<<"application">>,   <<"occi+xml">>,  []}, to_xml}
     ],
     Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"text">>,            <<"plain">>,     []}, from_plain},
      {{<<"text">>,            <<"occi">>,      []}, from_occi},
      {{<<"application">>,     <<"json">>,      []}, from_json},
      {{<<"application">>,     <<"occi+json">>, []}, from_json},
      {{<<"application">>,     <<"xml">>,       []}, from_xml},
      {{<<"application">>,     <<"occi+xml">>,  []}, from_xml}
     ],
     Req, State}.

delete_resource(Req, State) ->
    case get_cta(Req, State) of
	undefined ->
	    {ok, Req2} = cowboy_req:reply(415, Req),
	    {halt, Req2, State};
	Fun ->
	    ?MODULE:Fun(Req, State)
    end.

to_plain(Req, State) ->
    to(Req, State#state{ct=?ct_plain}).

to_occi(Req, State) ->
    to(Req, State#state{ct=?ct_occi}).

to_uri_list(Req, State) ->
    to(Req, State#state{ct=?ct_uri_list}).

to_json(Req, State) ->
    to(Req, State#state{ct=?ct_json}).

to_xml(Req, State) ->
    to(Req, State#state{ct=?ct_xml}).

from_plain(Req, State) ->
    from(Req, State#state{ct=?ct_plain}).

from_occi(Req, State) ->
    from(Req, State#state{ct=?ct_occi}).

from_json(Req, State) ->
    from(Req, State#state{ct=?ct_json}).

from_xml(Req, State) ->
    from(Req, State#state{ct=?ct_xml}).

%%%
%%% Private
%%%
to(Req, #state{ct=#content_type{renderer=R}, node=Node}=State) ->
    {ok, [Node2]} = occi_store:find(Node),
    {Body, Req2} = R:render(Node2, Req),
    {Body, Req2, State}.

from(Req, #state{ct=#content_type{parser=Parser}, op=Op}=State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case Parser:parse_user_mixin(Body, Req2) of
	{error, {parse_error, Err}} ->
	    lager:debug("Error processing request: ~p~n", [Err]),
	    {ok, Req3} = cowboy_req:reply(400, Req2),
	    {halt, Req3, State};
	{error, Err} ->
	    lager:debug("Internal error: ~p~n", [Err]),
	    {halt, Req2, State};	    
	{ok, undefined} ->
	    lager:debug("Empty request~n"),
	    {false, Req2, State};
	{ok, #occi_mixin{id=Cid, location=Uri}=Mixin} ->
	    case Op of
		delete ->
		    case occi_store:find(#occi_mixin{id=Cid, _='_'}) of
			{ok, []} ->
			    {ok, Req3} = cowboy_req:reply(404, Req2),
			    {halt, Req3, State};
			{ok, [#occi_mixin{user=true}]} ->
			    {ok, Req3} = cowboy_req:reply(403, Req2),
			    {halt, Req3, State};
			{ok, [Mixin2]} ->
			    case occi_store:delete(Mixin2) of
				{error, undefined_backend} ->
				    lager:debug("Internal error deleting user mixin~n"),
				    {halt, Req, State};
				{error, Reason} ->
				    lager:debug("Error deleting user mixin: ~p~n", [Reason]),
				    {false, Req, State};
				ok ->
				    {true, cowboy_req:set_resp_body("OK\n", Req2), State}
			    end
		    end;
		update ->
		    case occi_store:save(Mixin) of
			ok ->
			    Req3 = cowboy_req:set_resp_header(<<"location">>, occi_uri:to_binary(Uri), Req2),
			    {true, cowboy_req:set_resp_body("OK\n", Req3), State};
			{error, Reason} ->
			    lager:debug("Error creating resource: ~p~n", [Reason]),
			    {halt, Req2, State}
		    end
	    end
    end.

get_cta(Req, State) ->
    case cowboy_req:parse_header(<<"content-type">>, Req) of
	{error, badarg} ->
	    undefined;
	{ok, undefined, Req2} ->
	    {[{_Type, Fun}|_CTA], _, _} = content_types_accepted(Req2, State),
	    Fun;
	{ok, ContentType, Req2} ->
	    {CTA, _, _} = content_types_accepted(Req2, State),
	    choose_cta(ContentType, CTA)
    end.

choose_cta(_ContentType, []) ->
    undefined;
choose_cta(ContentType, [{Accepted, Fun}|_Tail]) 
  when Accepted =:= '*'; Accepted =:= ContentType ->
    Fun;
choose_cta({Type, SubType, Param}, [{{Type, SubType, AcceptedParam}, Fun}|_Tail]) 
  when AcceptedParam =:= '*'; AcceptedParam =:= Param ->
    Fun;
choose_cta(ContentType, [_Any|Tail]) ->
    choose_cta(ContentType, Tail).
