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
-module(occi_xmpp_handler).

-include("occi_xmpp.hrl").
-include("occi.hrl").
-include("occi_log.hrl").
-include_lib("erim/include/erim.hrl").
-include_lib("erim/include/erim_client.hrl").

-export([init/3,
         handle_iq/2]).

-record(state, {ref      :: pid(),
                jid      :: #jid{},
                from     :: #jid{},
                op       :: atom(),
                node     :: occi_node(),
                exists   :: boolean(),
                env      :: occi_env()}).


-define(caps, #occi_node{type=capabilities}).
-define(payload_path, [{element, ?ns_occi_xmpp, 'query'}]).

-spec init(HandlerOpts :: any(), Opts  :: term(), Ref :: pid()) -> {ok, #state{}}.
init(_HandlerOpts, Opts, Ref) ->
    Jid = case proplists:get_value(creds, Opts) of
              {#jid{}=J, _} -> J;
              {local, #jid{}=J} -> J
          end,
    {ok, #state{ref=Ref, jid=Jid}}.


-spec handle_iq(Iq :: #received_packet{}, State :: #state{}) -> {ok, NewState :: #state{}} 
                                                                    | {error, Error :: term()}.
handle_iq(#received_packet{from={Node, Domain, _Res}, raw_packet=Raw}, #state{jid=Jid}=S) ->
    Raw2 = erim_xml:remove_whitespaces_deeply(Raw),
    case get_op(Raw2) of
        {ok, Op} ->
            Endpoint = #uri{scheme='xmpp+occi', userinfo=Jid#jid.node, host=Jid#jid.domain},
            handle_filters(Raw2, S#state{op=Op, from=exmpp_jid:make({Node, Domain, undefined}), 
                                         env=#occi_env{req_uri=Endpoint}});
        error ->
            respond(Raw, S, 'bad-request'),
            terminate(S)
    end.

%%%
%%% Priv
%%%
handle_filters(#xmlel{}=Req, #state{op=Op}=S) 
  when read =:= Op orelse delete =:= Op ->
    case get_filters(Req) of
        {ok, Filters} ->
            lager:debug("Filters: ~p~n", [Filters]),
            handle_node(Filters, Req, S);
        {error, Err} ->
            lager:debug("Error parsing filters: ~p~n", [Err]),
            respond(Req, S, 'bad-request'),
            terminate(S)
    end;

handle_filters(#xmlel{}=Req, #state{}=S) ->
    handle_node([], Req, S).


handle_node(Filters, Req, S) ->
    case get_node(Filters, Req) of
        {ok, Node} ->
            resource_exists(Req, S#state{node=Node});
        {error, Err} ->
            lager:debug("Error getting node: ~p~n", [Err]),
            respond(Req, S, 'bad-request'),
            terminate(S)
    end.

resource_exists(#xmlel{}=Req, #state{node=#occi_node{type=capabilities}}=State) ->
    method(Req, State);
resource_exists(#xmlel{}=Req, #state{op=create, node=#occi_node{objid=undefined}}=State) ->
    method(Req, State#state{exists=false});
resource_exists(#xmlel{}=Req, #state{op=create, node=#occi_node{}}=State) ->
    method(Req, State#state{exists=true});
resource_exists(#xmlel{}=Req, #state{node=#occi_node{objid=undefined}}=State) ->
    respond(Req, State, 'item-not-found'),
    terminate(State);
resource_exists(#xmlel{}=Req, #state{node=#occi_node{}}=State) ->
    method(Req, State).


method(#xmlel{}=Req, #state{op=delete}=State) ->
    delete(Req, State);
method(#xmlel{}=Req, #state{op=create}=State) ->
    is_conflict(Req, State);
method(#xmlel{}=Req, #state{op=update}=State) ->
    update(Req, State);
method(#xmlel{}=Req, #state{op=read}=State) ->
    render(Req, State).


delete(#xmlel{}=Req, #state{node=Node, ref=Ref, from=From}=State) ->
    case occi_store:delete(Node,  #occi_store_ctx{user=From, auth_ref=Ref}) of
        {error, Err} ->
            lager:error("Error deleting: ~p~n", [Err]),
            respond(Req, State, 'internal-server-error'),
            terminate(State);
        ok ->
            respond(Req, State),
            terminate(State)
    end.


is_conflict(Req, #state{exists=false}=State) ->
    save(Req, State);

is_conflict(Req, #state{node=#occi_node{id=#uri{}, type=occi_resource}}=State) ->
    respond(Req, State, 'conflict'),
    terminate(State);

is_conflict(Req, #state{node=#occi_node{id=#uri{}, type=occi_link}}=State) ->
    respond(Req, State, 'conflict'),
    terminate(State);

is_conflict(Req, #state{node=#occi_node{type=occi_collection}, exists=true}=State) ->
    respond(Req, State, 'not-allowed'),
    terminate(State);

is_conflict(Req, State) ->
    save(Req, State).


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


update_entity(#xmlel{}=Req, #state{node=Node, ref=Ref, from=From}=State) ->
    case get_payload(Req) of
        undefined ->
            respond(Req, State, 'bad-request'),
            terminate(State);
        #xmlel{}=Body ->
            case occi_store:load(Node) of
                {ok, #occi_node{}=Node2} ->
                    case occi_parser_xml:parse_el(Body) of
                        {error, {parse_error, Err}} ->
                            lager:error("Error processing request: ~p~n", [Err]),
                            respond(Req, State, 'bad-request'),
                            terminate(State);
                        {error, Err} ->
                            lager:error("Internal error: ~p~n", [Err]),
                            respond(Req, State, 'internal-server-error'),
                            terminate(State);
                        {ok, #occi_resource{attributes = Attr}=_Res} ->
                            Data = occi_node:get_data(Node2),
                            Res2 = occi_resource:update_attr_value(Data, Attr),
                            Node3 = occi_node:set_data(Node2, Res2),
                            case occi_store:update(Node3, #occi_store_ctx{user=From, auth_ref=Ref}) of
                                ok ->
                                    respond(Req, State),
                                    terminate(State);
                                {error, Reason} ->
                                    lager:error("Error updating resource: ~p~n", [Reason]),
                                    respond(Req, State, 'internal-server-error'),
                                    terminate(State)
                            end;
                        {ok, #occi_link{attributes = Attr}=_Link} ->
                            Data = occi_node:get_data(Node2),
                            Link2 = occi_link:update_attr_value(Data, Attr),
                            Node3 = occi_node:set_data(Node2, Link2),
                            case occi_store:update(Node3, #occi_store_ctx{user=From, auth_ref=Ref}) of
                                ok ->
                                    respond(Req, State),
                                    terminate(State);
                                {error, Reason} ->
                                    lager:error("Error updating link: ~p~n", [Reason]),
                                    respond(Req, State, 'internal-server-error'),
                                    terminate(State)
                            end
                    end;	    
                {error, Err} ->
                    lager:error("Error loading object: ~p~n", [Err]),
                    respond(Req, State, 'internal-server-error'),
                    terminate(State)
            end
    end.


update_capabilities(Req, #state{env=Env, from=From, ref=Ref}=State) ->
    case get_payload(Req) of
        undefined ->
            respond(Req, State, 'bad-request'),
            terminate(State);
        #xmlel{}=Body ->
            case occi_parser_xml:parse_el(Body) of
                {error, {parse_error, Err}} ->
                    lager:debug("Error processing request: ~p~n", [Err]),
                    respond(Req, State, 'bad-request'),
                    terminate(State);
                {error, Err} ->
                    lager:debug("Internal error: ~p~n", [Err]),
                    respond(Req, State, 'internal-server-error'),
                    terminate(State);
                {ok, undefined} ->
                    lager:debug("Empty request~n"),
                    respond(Req, State, 'bad-request'),
                    terminate(State);
                {ok, #occi_mixin{}=Mixin} ->
                    Node = occi_node:new(Mixin, From),
                    case occi_store:update(Node, #occi_store_ctx{user=From, auth_ref=Ref}) of
                        ok ->
                            Req2 = set_payload(Req, occi_renderer_xml:to_xmlel(Node, Env)),
                            respond(Req2, State),
                            terminate(State);
                        {error, Reason} ->
                            lager:debug("Error creating resource: ~p~n", [Reason]),
                            respond(Req, State, 'internal-server-error'),
                            terminate(State)
                    end
            end
    end.


update_collection(Req, #state{node=#occi_node{objid=#occi_cid{class=kind}}}=State) ->
    respond(Req, State, 'not-allowed'),
    terminate(State);

update_collection(Req, #state{node=#occi_node{objid=Cid}=Node, from=From, ref=Ref}=State) ->
    case get_payload(Req) of
        undefined ->
            respond(Req, State, 'bad-request'),
            terminate(State);
        #xmlel{}=Body ->
            case occi_parser_xml:parse_el(Body) of
                {error, {parse_error, Err}} ->
                    lager:error("Error processing request: ~p~n", [Err]),
                    respond(Req, State, 'bad-request'),
                    terminate(State);
                {error, Err} ->
                    lager:error("Internal error: ~p~n", [Err]),
                    respond(Req, State, 'internal-server-error'),
                    terminate(State);
                {ok, #occi_collection{}=C} ->
                    Coll = occi_collection:new(Cid, occi_collection:get_entities(C)),
                    Node2 = Node#occi_node{type=occi_collection, data=Coll},
                    case occi_store:update(Node2, #occi_store_ctx{user=From, auth_ref=Ref}) of
                        ok ->
                            respond(Req, State),
                            terminate(State);
                        {error, {no_such_entity, Uri}} ->
                            lager:debug("Invalid entity: ~p~n", [lager:pr(Uri, ?MODULE)]),
                            respond(Req, State, 'bad-request'),
                            terminate(State);
                        {error, Reason} ->
                            lager:error("Error updating collection: ~p~n", [Reason]),
                            respond(Req, State, 'internal-server-error'),
                            terminate(State)
                    end
            end
    end.


render(Req, #state{node=Node, env=Env}=State) ->
    case occi_store:load(Node) of
        {ok, Node2} ->
            Req2 = set_payload(Req, occi_renderer_xml:to_xmlel(Node2, Env)),
            respond(Req2, State),
            terminate(State);
        {error, Err} ->
            lager:error("Internal error: ~p~n", [Err]),
            respond(Req, State, 'internal-server-error'),
            terminate(State)
    end.


save(Req, #state{node=#occi_node{type=occi_collection, objid=#occi_cid{class=Cls}}}=State) ->
    case Cls of
        kind ->
            respond(Req, State, 'not-allowed'),
            terminate(State);
        _ ->
            save_collection(Req, State)
    end;

save(Req, #state{node=#occi_node{type=occi_user_mixin}}=State) ->
    save_collection(Req, State);

save(Req, #state{node=#occi_node{type=occi_resource}}=State) ->
    save_entity(Req, State);

save(Req, #state{node=#occi_node{type=occi_link}}=State) ->
    save_entity(Req, State);

save(Req, #state{node=#occi_node{type=undefined}}=State) ->
    save_entity(Req, State).


save_collection(Req, #state{node=#occi_node{objid=Cid}=Node,
                            env=Env, from=From}=State) ->
    case get_payload(Req) of
        undefined ->
            respond(Req, State, 'bad-request'),
            terminate(State);
        #xmlel{}=Body ->
            case occi_parser_xml:parse_el(Body) of
                {error, {parse_error, Err}} ->
                    lager:error("Error processing request: ~p~n", [Err]),
                    respond(Req, State, 'bad-request'),
                    terminate(State);
                {error, Err} ->
                    lager:error("Internal error: ~p~n", [Err]),
                    respond(Req, State, 'internal-server-error'),
                    terminate(State);
                {ok, #occi_collection{}=C} ->
                    Coll = occi_collection:new(Cid, occi_collection:get_entities(C)),
                    case occi_store:save(Node#occi_node{type=occi_collection, data=Coll, owner=From}) of
                        ok ->
                            respond(Req, State),
                            terminate(State);
                        {error, {no_such_entity, Uri}} ->
                            lager:debug("Invalid entity: ~p~n", [occi_uri:to_string(Uri, Env)]),
                            respond(Req, State, 'bad-request'),
                            terminate(State);
                        {error, Reason} ->
                            lager:error("Error saving collection: ~p~n", [Reason]),
                            respond(Req, State, 'internal-server-error'),
                            terminate(State)
                    end
            end
    end.

%%TODO change parse or change code here
save_entity(Req, #state{from=From, node=Node, ref=Ref}=State) ->
    Path = erim_xml:get_element(Req, query),
    Path1 = erim_xml:get_attribute(Path, <<"node">>, "not found"),
    Path2 = binary_to_list(Path1),
    Uri = #uri{path = Path2},
    case get_payload(Req) of
        undefined ->
            respond(Req, State, 'bad-request'),
            terminate(State);
        #xmlel{} = Body ->
            case occi_parser_xml:parse_el(Body) of
                {error, {parse_error, Err}} ->
                    lager:error("Error processing request: ~p~n", [Err]),
                    respond(Req, State, 'bad-request'),
                    terminate(State);
                {error, Err} ->
                    lager:error("Internal error: ~p~n", [Err]),
                    respond(Req, State, 'internal-server-error'),
                    terminate(State);
                {ok, #occi_resource{}=Res} ->
                    Res2 = occi_resource:set_id(Res, Uri),
                    Node2 = occi_node:new(Res2, From),
                    case occi_store:save(Node2, #occi_store_ctx{user=Node#occi_node.owner, auth_ref=Ref}) of
                        ok ->
                            respond(Req, State),
                            terminate(State);
                        {error, Reason} ->
                            lager:error("Error creating resource: ~p~n", [Reason]),
                            respond(Req, State, 'internal-server-error'),
                            terminate(State)
                    end;
                {ok, #occi_link{}=Link} ->
                    Link2 = occi_link:set_id(Link, Uri),
                    Node2 = occi_node:new(Link2, From),
                    case occi_store:save(Node2, #occi_store_ctx{user=Node#occi_node.owner}) of
                        ok ->
                            respond(Req, State),
                            terminate(State);
                        {error, Reason} ->
                            lager:error("Error creating link: ~p~n", [Reason]),
                            respond(Req, State, 'internal-server-error'),
                            terminate(State)
                    end
            end
    end.


respond(#xmlel{children=[Q]}=Req, #state{ref=Ref}) ->
    case exmpp_iq:get_type(Req) of
        result ->
            erim_client:send(Ref, Req);
        error ->
            erim_client:send(Ref, Req);
        _ ->
            Res = erim_xml:append_child(exmpp_iq:result(Req), Q),
            erim_client:send(Ref, Res)
    end.


respond(#xmlel{}=Req, #state{ref=Ref}, Code) ->
    Err = exmpp_iq:error(Req, Code),
    erim_client:send(Ref, Err).


terminate(#state{ref=Ref, jid = Jid}) ->
    {ok, #state{ref=Ref, jid = Jid}}.

find_node(Path, _Filters) ->
    Url = occi_uri:parse(Path),
    case occi_store:find(#occi_node{id=#uri{path=Url#uri.path}, _='_'}) of
        {ok, []} -> #occi_node{id=Url};
        {ok, [#occi_node{}=N]} -> N
    end.


find_caps_node([#occi_cid{}=Cid]) ->
    case occi_store:find(#occi_node{type=capabilities, objid=Cid, _='_'}) of
        {ok, []} -> ?caps;
        {ok, [#occi_node{}=N]} -> N
    end;

find_caps_node(_) ->
    {ok, [#occi_node{}=N]} = occi_store:find(?caps),
    N.


get_op(#xmlel{children=[El]}=Iq) ->
    case exmpp_iq:get_type(Iq) of
        'get' -> {ok, read};
        'set' ->
            case erim_xml:get_attribute(El, <<"op">>, <<"save">>) of
                <<"save">> -> {ok, create};
                <<"update">> -> {ok, update};
                <<"delete">> -> {ok, delete};
                _ -> error
            end;
        _ -> error
    end;

get_op(_El) ->
    error.


get_filters(_) ->
    % TODO
    {ok, []}.


get_node(Filters, #xmlel{children=[El]}) ->
    case erim_xml:get_attribute(El, <<"type">>, <<"entity">>) of
        <<"caps">> ->
            {ok, find_caps_node(Filters)};
        <<"entity">> ->
            case erim_xml:get_attribute(El, <<"node">>, undefined) of
                undefined ->
                    {error, invalid_node_id};
                Path ->
                    {ok, find_node(Path, Filters)}
            end;
        _ ->
            {error, invalid_node_type}
    end.		

get_payload(#xmlel{}=E) ->
    case erim_xml:get_path(E, ?payload_path) of
        undefined -> undefined;
        #xmlel{children=[Child]} -> Child;
        _ -> undefined
    end.

set_payload(#xmlel{}=E, #xmlel{}=Payload) ->
    E#xmlel{children=[Payload]}.
