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

-module(occi_http_collection).
-compile({parse_transform, lager_transform}).

%% REST Callbacks
-export([init/3, 
	 rest_init/2,
	 allow_missing_post/2,
	 allowed_methods/2,
	 delete_resource/2,
	 content_types_provided/2,
	 content_types_accepted/2]).

%% Callback callbacks
-export([to_plain/2,
	 to_json/2,
	 to_occi/2,
	 to_uri_list/2,
	 to_xml/2]).
-export([from_json/2]).

-include("occi.hrl").

-record(state, {category     :: occi_category(),
		parse_status :: term()}).

init(_Transport, _Req, _) -> 
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, Cat) ->
    Req1 = occi_http:set_cors(Req),
    {ok, Req1, #state{category=Cat}}.

allow_missing_post(_Req, _State) ->
    false.

allowed_methods(Req, #state{category=#occi_kind{}}=State) ->
    {[<<"HEAD">>, <<"GET">>, <<"DELETE">>, <<"POST">>, <<"OPTIONS">>], Req, State};
allowed_methods(Req, #state{category=#occi_mixin{}}=State) ->
    {[<<"HEAD">>, <<"GET">>, <<"PUT">>, <<"DELETE">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"text">>,          <<"plain">>,       []}, to_plain},
      {{<<"text">>,          <<"occi">>,        []}, to_occi},
      {{<<"text">>,          <<"uri-list">>,    []}, to_uri_list},
      {{<<"application">>,   <<"json">>,        []}, to_json},
      {{<<"application">>,   <<"occi+json">>,   []}, to_json},
      {{<<"application">>,   <<"xml">>,         []}, to_xml},
      {{<<"application">>,   <<"occi+xml">>,    []}, to_xml}
     ],
     Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>,     <<"json">>,      []}, from_json},
      {{<<"application">>,     <<"occi+json">>, []}, from_json}
     ],
     Req, State}.

delete_resource(Req, #state{category=Cat}=State) ->
    {ok, Coll} = occi_store:get_collection(Cat),
    case occi_store:delete(Coll) of
	{error, undefined_backend} ->
	    lager:debug("Internal error deleting entities~n"),
	    {ok, Req2} = cowboy_req:reply(500, Req),
	    {halt, Req2, State};
	{error, Reason} ->
	    lager:debug("Error deleting entities: ~p~n", [Reason]),
	    {false, Req, State};
	ok ->
	    {true, Req, State}
    end.

to_plain(Req, #state{category=Cat}=State) ->
    {ok, Coll} = occi_store:get_collection(Cat),
    Body = [occi_renderer_plain:render_collection(Coll), "\n"],
    {Body, Req, State}.

to_occi(Req, #state{category=Cat}=State) ->
    {ok, Coll} = occi_store:get_collection(Cat),
    Req2 = cowboy_req:set_resp_header(<<"x-occi-location">>, 
				      occi_renderer_occi:render_collection(Coll), Req),
    Body = <<"OK\n">>,
    {Body, Req2, State}.

to_uri_list(Req, #state{category=Cat}=State) ->
    {ok, Coll} = occi_store:get_collection(Cat),
    Body = [occi_renderer_uri_list:render_collection(Coll), "\n"],
    {Body, Req, State}.

to_json(Req, #state{category=Cat}=State) ->
    {ok, Coll} = occi_store:get_collection(Cat),
    Body = [occi_renderer_json:render_collection(Coll), "\n"],
    {Body, Req, State}.

to_xml(Req, #state{category=Cat}=State) ->
    {ok, Coll} = occi_store:get_collection(Cat),
    Body = [occi_renderer_xml:render_collection(Coll), "\n"],
    {Body, Req, State}.

from_json(Req, #state{category=#occi_kind{backend=Backend}=Kind}=State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case occi_parser_json:parse_resource(Body, Kind) of
	{error, Reason} ->
	    lager:debug("Error processing request: ~p~n", [Reason]),
	    {false, Req2, State};
	{ok, #occi_resource{}=Res} ->
	    {Prefix, Req3} = cowboy_req:path(Req2),
	    Res2 = occi_resource:set_id(Res, occi_config:gen_id(Prefix)),
	    case occi_store:save(Backend, Res2) of
		ok ->
		    RespBody = occi_renderer_json:render_entity(Res2),
		    {true, cowboy_req:set_resp_body([RespBody, "\n"], Req3), State};
		{error, Reason} ->
		    lager:debug("Error creating resource: ~p~n", [Reason]),
		    throw({error, Reason})
	    end
    end;
from_json(Req, #state{category=#occi_mixin{id=Id, backend=Backend}}=State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case occi_parser_json:parse_collection(Body) of
	{error, Reason} ->
	    lager:debug("Error processing request: ~p~n", [Reason]),
	    {false, Req2, State};
	{ok, #occi_collection{}=C} ->
	    Entities = occi_collection:get_entities(C),
	    case cowboy_req:method(Req2) of
		{<<"PUT">>, _} ->
		    case occi_store:associate_mixin(Backend, Id, Entities) of
			ok ->
			    {true, Req, State};
			{error, {no_such_entity, Uri}} ->
			    lager:debug("Invalid entity: ~p~n", [occi_uri:to_string(Uri)]),
			    {false, Req2, State};
			{error, Reason} ->
			    lager:debug("Error saving collection: ~p~n", [Reason]),
			    throw({error, Reason})
		    end;			   
		{<<"POST">>, _} ->
		    case occi_store:associate_mixin(Backend, Id, Entities) of
			ok ->
			    {true, Req, State};
			{error, {no_such_entity, Uri}} ->
			    lager:debug("Invalid entity: ~p~n", [occi_uri:to_string(Uri)]),
			    {false, Req2, State};
			{error, Reason} ->
			    lager:debug("Error updating collection: ~p~n", [Reason]),
			    throw({error, Reason})
		    end
	    end
    end.
