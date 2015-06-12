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

-module(occi_http_handler).

-include("occi.hrl").
-include("occi_log.hrl").
-include("occi_http.hrl").

%% REST Callbacks
-export([init/3, 
         rest_init/2,
         allowed_methods/2,
         allow_missing_post/2,
         is_authorized/2,
         resource_exists/2,
         is_conflict/2,
         delete_resource/2,
         content_types_provided/2,
         content_types_accepted/2]).

%% Callback callbacks
-export([to_occi/2,
         to_plain/2,
         to_uri_list/2,
         to_json/2,
         to_xml/2,
         from_plain/2,
         from_occi/2,
         from_json/2,
         from_xml/2]).

-record(state, {op      :: atom(), 
                node, 
                ct, 
                user,
                filters :: term(),
                env     :: occi_env(),
                auth    :: reference() | undefined}).

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

-define(caps, #occi_node{id=#uri{path="/-/"}, type=capabilities}).

init(_Transport, _Req, _Opts) -> 
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
    Op = occi_http_common:get_acl_op(Req),
    {Path, _} = cowboy_req:path(Req),
    Filters = case Op of
                  read -> parse_filters(Req);
                  delete -> parse_filters(Req);
                  _ -> []
              end,
    Node = case Path of
               <<"/-/">> ->
                   get_caps_node(Filters);
               <<"/.well-known/org/ogf/occi/-/">> ->
                   get_caps_node(Filters);
               _ ->
                   get_node(Path)
           end,
    {ok, cowboy_req:set_resp_header(<<"server">>, ?SERVER_ID, Req), 
     #state{op=Op, node=Node, filters=Filters, 
            env=#occi_env{req_uri=get_req_url(Req)},
            auth=proplists:get_value(auth, Opts)}}.

allowed_methods(Req, #state{node=#occi_node{objid=#uri{}, type=occi_collection}}=State) ->
    set_allowed_methods([<<"GET">>, <<"DELETE">>, <<"OPTIONS">>], Req, State);
allowed_methods(Req, #state{node=#occi_node{type=capabilities}}=State) ->
    set_allowed_methods([<<"GET">>, <<"DELETE">>, <<"OPTIONS">>, <<"POST">>], Req, State);
allowed_methods(Req, #state{node=#occi_node{objid=#occi_cid{class=kind}, type=occi_collection}}=State) ->
    set_allowed_methods([<<"GET">>, <<"DELETE">>, <<"OPTIONS">>, <<"POST">>], Req, State);
allowed_methods(Req, State) ->
    set_allowed_methods([<<"GET">>, <<"DELETE">>, <<"OPTIONS">>, <<"POST">>, <<"PUT">>], Req, State).

content_types_provided(Req, State) ->
    {[
      {{<<"text">>,            <<"plain">>,     []}, to_plain},
      {{<<"text">>,            <<"occi">>,      []}, to_occi},
      {{<<"text">>,            <<"uri-list">>,  []}, to_uri_list},
      {{<<"application">>,     <<"json">>,      []}, to_json},
      {{<<"application">>,     <<"occi+json">>, []}, to_json},
      {{<<"application">>,     <<"xml">>,       []}, to_xml},
      {{<<"application">>,     <<"occi+xml">>,  []}, to_xml}
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


allow_missing_post(Req, State) ->
    {false, Req, State}.


resource_exists(Req, #state{node=#occi_node{objid=undefined}}=State) ->
    {false, Req, State};

resource_exists(Req, State) ->
    {true, Req, State}.

is_authorized(Req, #state{auth=Ref}=State) ->
    case occi_http_common:auth(Ref, Req) of
        {true, User} ->
            {true, Req, State#state{user=User}};
        false ->
            {true, Req, State#state{user=anonymous}}
    end.

is_conflict(Req, #state{node=#occi_node{id=#uri{}, type=occi_resource}}=State) ->
    {true, Req, State};
is_conflict(Req, #state{node=#occi_node{id=#uri{}, type=occi_link}}=State) ->
    {true, Req, State};
is_conflict(Req, State) ->
    {false, Req, State}.


delete_resource(Req, #state{node=#occi_node{type=capabilities, data=undefined}}=State) ->
    {true, Req, State};

delete_resource(Req, #state{node=Node, user=User, auth=Ref}=State) ->
    case occi_store:delete(Node, #occi_store_ctx{user=User, auth_ref=Ref}) of
        {error, Reason} ->
            ?error("Error deleting node: ~p~n", [Reason]),
            {ok, Req2} = cowboy_req:reply(Reason, Req),
            {halt, Req2, State};
        ok ->
            {true, Req, State}
    end.

to_occi(Req, State) ->
    render(Req, State#state{ct=?ct_occi}).

to_plain(Req, State) ->
    render(Req, State#state{ct=?ct_plain}).

to_uri_list(Req, #state{node=#occi_node{type=capabilities}}=State) ->
    render(Req, State#state{ct=?ct_uri_list});
to_uri_list(Req, #state{node=#occi_node{type=occi_collection}}=State) ->
    render(Req, State#state{ct=?ct_uri_list});
to_uri_list(Req, State) ->
    {ok, Req2} = cowboy_req:reply(400, Req),
    {halt, Req2, State}.

to_json(Req, State) ->
    render(Req, State#state{ct=?ct_json}).

to_xml(Req, State) ->
    render(Req, State#state{ct=?ct_xml}).

from_plain(Req, State) ->
    save_or_update(Req, State#state{ct=?ct_plain}).

from_occi(Req, State) ->
    save_or_update(Req, State#state{ct=?ct_occi}).

from_json(Req, State) ->
    save_or_update(Req, State#state{ct=?ct_json}).

from_xml(Req, State) ->
    save_or_update(Req, State#state{ct=?ct_xml}).

%%%
%%% Private
%%%
save_or_update(Req, #state{op=create}=State) ->
    save(Req, State);
save_or_update(Req, #state{op=update}=State) ->
    update(Req, State);
save_or_update(Req, #state{op={action, Action}}=State) ->
    action(Req, State, Action).


save(Req, #state{node=#occi_node{type=occi_collection, objid=#occi_cid{class=Cls}}}=State) ->
    case Cls of
        kind ->
            save_entity(Req, State);
        _ ->
            save_collection(Req, State)
    end;

save(Req, #state{node=#occi_node{type=occi_resource}}=State) ->
    save_entity(Req, State);

save(Req, #state{node=#occi_node{type=occi_link}}=State) ->
    save_entity(Req, State);

save(Req, #state{node=#occi_node{type=undefined}}=State) ->
    save_entity(Req, State).


update(Req, #state{node=#occi_node{type=capabilities}}=State) ->
    update_capabilities(Req, State);

update(Req, #state{node=#occi_node{type=occi_collection, objid=#occi_cid{}}}=State) ->
    update_collection(Req, State);

update(Req, #state{node=#occi_node{type=occi_resource}}=State) ->
    update_entity(Req, State);

update(Req, #state{node=#occi_node{type=occi_link}}=State) ->
    update_entity(Req, State);

update(Req, #state{node=#occi_node{type=undefined}}=State) ->
    update_entity(Req, State).


render(Req, #state{node=Node, ct=#content_type{renderer=Renderer}, filters=Filters, 
                   user=User, env=Env, auth=AuthRef}=State) ->
    try parse_load_opts(Req) of
        {D, L, M} ->
            Opts = #occi_store_opts{entity_f=Filters, deep=D, limit=L, marker=M},
            Ctx = #occi_store_ctx{user=User, auth_ref=AuthRef},
            case occi_store:load(Node, Opts, Ctx) of
                {ok, #occi_node{type=occi_collection, data=Coll}=Node2} ->
                    {Body, #occi_env{req=Req2}} = Renderer:render(Node2, Env#occi_env{req=Req}),
                    {[Body, "\n"], render_collection(Coll, Req2), State};
                {ok, Node2} ->
                    {Body, #occi_env{req=Req2}} = Renderer:render(Node2, Env#occi_env{req=Req}),
                    {[Body, "\n"], Req2, State};
                {error, Err} ->
                    ?error("Error loading object: ~p~n", [Err]),
                    {ok, Req2} = cowboy_req:reply(Err, Req),
                    {halt, Req2, State}
            end
    catch _:_ ->
              {ok, Req2} = cowboy_req:reply(400, Req),
              {halt, Req2, State}
    end.


render_collection(#occi_collection{range=undefined}, Req) ->
    Req;
render_collection(#occi_collection{marker=Marker, range=Range}, Req) ->
    Req2 = cowboy_req:set_resp_header(<<"content-range">>, render_range(Range), Req),
    Req3 = cowboy_req:set_resp_header(<<"accept-ranges">>, <<"entity">>, Req2),
    case Marker of
        undefined ->
            Req3;
        _ ->
            {NextUri, Req4} = update_qs_val(<<"marker">>, Marker, Req3),
            NextLink = <<"<", (occi_uri:to_binary(NextUri#uri{'query'=""}))/binary, ">;next=", 
                         (occi_uri:to_binary(NextUri))/binary>>,
            cowboy_req:set_resp_header(<<"link">>, NextLink, Req4)
    end.


render_range({S, E, 0}) when is_integer(S),
                             is_integer(E) ->
    iolist_to_binary([<<"entity ">>, integer_to_binary(S), <<"-">>, integer_to_binary(E)]);
render_range({S, E, T}) when is_integer(S),
                             is_integer(E),
                             is_integer(T) ->
    iolist_to_binary([<<"entity ">>, 
                      integer_to_binary(S), <<"-">>, 
                      integer_to_binary(E), <<"/">>, 
                      integer_to_binary(T)]).


save_entity(Req, #state{env=Env, user=User, node=Node, auth=Ref, ct=#content_type{parser=Parser}}=State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Entity = prepare_entity(Node, Env),
    case Parser:parse_entity(Body, Req2, Entity) of
        {error, {parse_error, Err}} ->
            ?error("Error processing request: ~p~n", [Err]),
            {false, Req2, State};
        {error, Err} ->
            ?error("Internal error: ~p~n", [Err]),
            {halt, Req2, State};
        {ok, #occi_resource{}=Res} ->
            Node2 = occi_node:new(Res, User),
            case occi_store:save(Node2, #occi_store_ctx{user=Node#occi_node.owner, auth_ref=Ref}) of
                ok ->
                    {true, Req2, State};
                {error, Reason} ->
                    ?error("Error creating resource: ~p~n", [Reason]),
                    {ok, Req3} = cowboy_req:reply(Reason, Req2),
                    {halt, Req3, State}
            end;
        {ok, #occi_link{}=Link} ->
            Node2 = occi_node:new(Link, User),
            case occi_store:save(Node2, #occi_store_ctx{user=Node#occi_node.owner, auth_ref=Ref}) of
                ok ->
                    {true, Req2, State};
                {error, Reason} ->
                    ?error("Error creating link: ~p~n", [Reason]),
                    {ok, Req3} = cowboy_req:reply(Reason, Req2),
                    {halt, Req3, State}
            end
    end.


update_entity(Req, #state{node=Node, auth=Ref, user=User, ct=#content_type{parser=Parser}}=State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case occi_store:load(Node) of
        {ok, #occi_node{data=Entity}=Node2} ->
            case Parser:parse_entity(Body, Req2, Entity) of
                {error, {parse_error, Err}} ->
                    ?error("Error processing request: ~p~n", [Err]),
                    {false, Req2, State};
                {error, Err} ->
                    ?error("Internal error: ~p~n", [Err]),
                    {halt, Req2, State};	    
                {ok, #occi_resource{}=Res} ->
                    Node3 = occi_node:set_data(Node2, Res),
                    case occi_store:update(Node3, #occi_store_ctx{user=User, auth_ref=Ref}) of
                        ok ->
                            {true, cowboy_req:set_resp_body("OK\n", Req2), State};
                        {error, Reason} ->
                            ?error("Error updating resource: ~p~n", [Reason]),
                            {ok, Req3} = cowboy_req:reply(Reason, Req2),
                            {halt, Req3, State}
                    end;
                {ok, #occi_link{}=Link} ->
                    Node3 = occi_node:set_data(Node2, Link),
                    case occi_store:update(Node3, #occi_store_ctx{user=User, auth_ref=Ref}) of
                        ok ->
                            {true, cowboy_req:set_resp_body("OK\n", Req2), State};
                        {error, Reason} ->
                            ?error("Error updating link: ~p~n", [Reason]),
                            {ok, Req3} = cowboy_req:reply(Reason, Req2),
                            {halt, Req3, State}
                    end
            end;	    
        {error, Err} ->
            ?error("Error loading object: ~p~n", [Err]),
            {halt, Req2, State}
    end.


save_collection(Req, #state{ct=#content_type{parser=Parser},
                            node=#occi_node{objid=Cid}=Node,
                            env=Env}=State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case Parser:parse_collection(Body, Req2) of
        {error, {parse_error, Err}} ->
            ?error("Error processing request: ~p~n", [Err]),
            {false, Req2, State};
        {error, Err} ->
            ?error("Internal error: ~p~n", [Err]),
            {halt, Req2, State};	    
        {ok, #occi_collection{}=C} ->
            Coll = occi_collection:new(Cid, occi_collection:get_entities(C)),
            case occi_store:save(Node#occi_node{type=occi_collection, data=Coll}) of
                ok ->
                    {true, Req, State};
                {error, {no_such_entity, Uri}} ->
                    ?debug("Invalid entity: ~p~n", [occi_uri:to_string(Uri, Env)]),
                    {false, Req2, State};
                {error, Reason} ->
                    ?error("Error saving collection: ~p~n", [Reason]),
                    {halt, Req2, State}
            end
    end.


update_collection(Req, #state{ct=#content_type{parser=Parser}, env=Env,
                              user=User, auth=Ref,
                              node=#occi_node{type=occi_collection, objid=#occi_cid{class=kind}}=Node}=State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Entity = prepare_entity(Node, Env),
    case Parser:parse_entity(Body, Req2, Entity) of
        {error, {parse_error, Err}} ->
            ?error("Error processing request: ~p~n", [Err]),
            {false, Req2, State};
        {error, Err} ->
            ?error("Internal error: ~p~n", [Err]),
            {halt, Req2, State};
        {ok, #occi_resource{}=Res} ->
            Node2 = occi_node:new(Res, User),
            case occi_store:save(Node2, #occi_store_ctx{user=Node#occi_node.owner, auth_ref=Ref}) of
                ok ->
                    {{true, occi_uri:to_binary(Node2#occi_node.id, Env)}, Req2, State};
                {error, Reason} ->
                    ?error("Error creating resource: ~p~n", [Reason]),
                    {halt, Req2, State}
            end;
        {ok, #occi_link{}=Link} ->
            Node2 = occi_node:new(Link, User),
            case occi_store:save(Node2, #occi_store_ctx{user=Node#occi_node.owner, auth_ref=Ref}) of
                ok ->
                    {{true, occi_uri:to_binary(Node2#occi_node.id, Env)}, Req2, State};
                {error, Reason} ->
                    ?error("Error creating link: ~p~n", [Reason]),
                    {halt, Req2, State}
            end
    end;

update_collection(Req, #state{auth=Ref, user=User, ct=#content_type{parser=Parser}, node=#occi_node{objid=Cid}=Node}=State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case Parser:parse_collection(Body, Req2) of
        {error, {parse_error, Err}} ->
            ?error("Error processing request: ~p~n", [Err]),
            {false, Req2, State};
        {error, Err} ->
            ?error("Internal error: ~p~n", [Err]),
            {halt, Req2, State};	    
        {ok, #occi_collection{}=C} ->
            Coll = occi_collection:new(Cid, occi_collection:get_entities(C)),
            Node2 = Node#occi_node{type=occi_collection, data=Coll},
            case occi_store:update(Node2, #occi_store_ctx{user=User, auth_ref=Ref}) of
                ok ->
                    {true, Req, State};
                {error, {unknown_entity, Uri}} ->
                    ?debug("Invalid entity: ~p~n", [Uri]),
                    {false, Req2, State};
                {error, Reason} ->
                    ?error("Error updating collection: ~p~n", [Reason]),
                    {halt, Req2, State}
            end
    end.


update_capabilities(Req, #state{env=Env, user=User, auth=Ref, ct=#content_type{parser=Parser}}=State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case Parser:parse_user_mixin(Body, Req2) of
        {error, {parse_error, Err}} ->
            ?debug("Error processing request: ~p~n", [Err]),
            {ok, Req3} = cowboy_req:reply(400, Req2),
            {halt, Req3, State};
        {error, Err} ->
            ?debug("Internal error: ~p~n", [Err]),
            {halt, Req2, State};	    
        {ok, undefined} ->
            ?debug("Empty request~n"),
            {false, Req2, State};
        {ok, #occi_mixin{location=Uri}=Mixin} ->
            case occi_store:update(occi_node:new(Mixin, User), #occi_store_ctx{user=User, auth_ref=Ref}) of
                ok ->
                    {{true, occi_uri:to_binary(Uri, Env)}, cowboy_req:set_resp_body("OK\n", Req2), State};
                {error, Reason} ->
                    ?debug("Error creating resource: ~p~n", [Reason]),
                    {halt, Req2, State}
            end
    end.


action(Req, #state{ct=#content_type{parser=Parser}, node=Node}=State, ActionName) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case Parser:parse_action(Body, Req2, prepare_action(Req2, Node, ActionName)) of
        {error, {parse_error, Err}} ->
            ?error("Error processing action: ~p~n", [Err]),
            {false, Req2, State};
        {error, Err} ->
            ?error("Internal error: ~p~n", [Err]),
            {halt, Req2, State};	    
        {ok, #occi_action{}=Action} ->
            case occi_store:action(Node, Action) of
                ok ->
                    {true, Req2, State};
                {error, Err} ->
                    ?error("Error triggering action: ~p~n", [Err]),
                    {halt, Req2, Node}
            end
    end.


prepare_action(_Req, _Node, Name) ->
    occi_action:new(#occi_cid{term=?term_to_atom(Name), class=action}).


prepare_entity(#occi_node{id=#uri{path=Prefix}, type=occi_collection, objid=Cid}, Env) ->
    Id = occi_config:gen_id(Prefix, Env),
    case occi_store:get(Cid) of
        {ok, #occi_kind{parent=#occi_cid{term=resource}}=Kind} ->
            occi_resource:new(Id, Kind);
        {ok, #occi_kind{parent=#occi_cid{term=link}}=Kind} ->
            occi_link:new(Id, Kind);
        _ ->
            throw({error, internal_error})
    end;

prepare_entity(#occi_node{type=occi_resource}=Node, _Env) ->
    case occi_store:load(Node) of
        {ok, #occi_node{data=Res}} ->
            occi_resource:reset(Res);
        {error, Err} ->
            throw({error, Err})
    end;

prepare_entity(#occi_node{type=occi_link}=Node, _Env) ->
    case occi_store:load(Node) of
        {ok, #occi_node{data=Link}} ->
            occi_link:reset(Link);
        {error, Err} ->
            throw({error, Err})
    end;

prepare_entity(#occi_node{id=Id, type=undefined}, _Env) ->
    #occi_entity{id=Id}.

set_allowed_methods(Methods, Req, State) ->
    << ", ", Allow/binary >> = << << ", ", M/binary >> || M <- Methods >>,
    {Methods, occi_http_common:set_cors(Req, Allow), State}.

parse_filters(Req) ->
    case cowboy_req:qs_val(<<"category">>, Req) of
        {undefined, _} ->
            parse_attr_filters([], Req);
        {Bin, _} ->
            parse_category_filter(Bin, Req)
    end.

parse_category_filter(Bin, Req) ->
    case binary:split(occi_uri:decode(Bin), <<"#">>) of
        [Scheme, Term] ->
            S = ?scheme_to_atom(<<Scheme/binary, $#>>),
            T = ?term_to_atom(Term),
            parse_attr_filters([#occi_cid{scheme=S, term=T, _='_'}], Req);
        _ -> []
    end.

parse_attr_filters(Acc, Req) ->
    case cowboy_req:qs_val(<<"q">>, Req) of
        {undefined, _} -> Acc;
        {Enc, _} ->
            parse_attr_filter(binary:split(Enc, <<"+">>), Acc)
    end.

parse_attr_filter([], Acc) ->
    lists:reverse(Acc);
parse_attr_filter([ Attr | Rest ], Acc) ->
    case binary:split(occi_uri:decode(Attr), <<"=">>) of
        [] -> parse_attr_filter(Rest, Acc);
        [Val] -> parse_attr_filter(Rest, [{like, '_', Val} | Acc]);
        [Name, Val] -> parse_attr_filter(Rest, [ {'=:=', ?attr_to_atom(Name), Val} | Acc ])
    end.

parse_load_opts(Req) ->
    parse_marker(Req, {false, -1, 0}).

parse_marker(Req, {D, L, _}) ->
    case cowboy_req:qs_val(<<"marker">>, Req) of
        {undefined, _} -> parse_limit(Req, {D, L, 0});
        {Bin, _} -> parse_limit(Req, {D, L, Bin})
    end.

parse_limit(Req, {D, _, M}) ->
    case cowboy_req:qs_val(<<"limit">>, Req) of
        {undefined, _} -> parse_deep(Req, {D, -1, M});
        {Bin, _} -> parse_deep(Req, {D, binary_to_integer(Bin), M})
    end.

parse_deep(Req, {_, L, M}) ->
    case cowboy_req:qs_val(<<"deep">>, Req) of
        {true, _} -> {true, L, M};
        {<<"1">>, _} -> {true, L, M};
        _ -> {false, L, M}
    end.

get_node(Path) ->
    Url = occi_uri:parse(Path),
    case occi_store:find(#occi_node{id=#uri{path=Url#uri.path}, _='_'}) of
        {ok, []} -> #occi_node{id=Url};
        {ok, [#occi_node{}=N]} -> N;
        {error, Err} ->
            ?error("Error looking for node:~n~p~n", [Err]),
            throw({error, Err})
    end.

get_caps_node([#occi_cid{}=Cid]) ->
    Node = #occi_node{id=#uri{path="/-/"}, type=capabilities, objid=Cid, _='_'},
    case occi_store:find(Node) of
        {ok, []} -> 
            ?debug("No such mixin: ~p~n", [Cid]),
            Node#occi_node{data=undefined};
        {ok, [#occi_node{}=N]} -> 
            ?debug("Found capabilities node: ~p~n", [N]),
            N
    end;
get_caps_node(_) ->
    {ok, [#occi_node{}=N]} = occi_store:find(?caps),
    N.

get_req_url(Req) ->
    {ReqUrl, _} = cowboy_req:url(Req),
    occi_uri:parse(ReqUrl).

update_qs_val(Name, Value, Req) ->
    {Host, _} = cowboy_req:host_url(Req),
    {Path, _} = cowboy_req:path(Req),
    {QsVals, Req2} = cowboy_req:qs_vals(Req),
    {occi_uri:new(Host, Path, lists:keystore(Name, 1, QsVals, {Name, Value})), Req2}.
