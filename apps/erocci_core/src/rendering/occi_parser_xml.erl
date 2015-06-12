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
%%% Created :  7 Oct 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_parser_xml).

-behaviour(gen_fsm).

-include("occi.hrl").
-include("occi_xml.hrl").
-include("occi_parser.hrl").
-include_lib("erim_xml/include/erim_xml.hrl").

%% API
-export([load_extension/1,
	 parse_extension/1,
         parse_el/1,
         parse_full/1,
         parse_action/3,
         parse_entity/3,
         parse_user_mixin/2,
         parse_collection/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% States
-export([init/3, 
         resource/3,
         link/3,
         extension/3,
         collection/3,
         kind/3,
         action/3,
         eof/3, 
         attribute_spec/3,
         mixin/3,
         action_spec/3]).

-define(SERVER, ?MODULE).
-define(PARSER_OPTIONS,
        [{engine, expat},
         {names_as_atom, true},
         {check_nss, xmpp},
         {check_elems, xmpp},
         {emit_endtag, true},
         {root_depth, none},
         {max_size, infinity}]).

-record(state, {
                request     = #occi_request{}     :: occi_request(),
                collection                        :: occi_collection(),
                entity      = undefined           :: term(),
                entity_id   = undefined           :: uri(),
                link        = undefined           :: occi_link(),
                mixin       = undefined           :: term(),
                action      = undefined           :: occi_action(),
                prefixes,
                extension                         :: occi_extension(),
                kind                              :: occi_kind(),
                attribute                         :: occi_attr(),
                type}).

%%%===================================================================
%%% API
%%%===================================================================
parse_el(#xmlel{}=El) ->
    case parse_full(El, #state{}) of
        {ok, Res} -> {ok, Res};
        {error, Reason} ->
            ?error("Error parsing xml: ~p~n", [Reason]),
            {error, {parse_error, Reason}}
    end.    

parse_full(Data) ->
    parse_full(Data, #state{request=#occi_request{}}).

load_extension(Path) ->
    {ok, Data} = file:read_file(Path),
    parse_extension(Data, []).

parse_extension(Data) ->
    parse_extension(Data, []).

parse_action(Data, _Env, Action) ->
    case parse_full(Data, #state{action=Action}) of
        {error, Reason} ->
            {error, {parse_error, Reason}};
        {ok, #occi_action{}=Action2} ->
            {ok, Action2};
        _ ->
            {error, {parse_error, not_an_action}}
    end.    

parse_entity(Data, _Env, #occi_resource{}=Res) ->
    case parse_full(Data, #state{entity=Res}) of
        {error, Reason} ->
            {error, {parse_error, Reason}};
        {ok, #occi_resource{}=Res2} ->
            {ok, Res2};
        _ ->
            {error, {parse_error, not_an_entity}}
    end;

parse_entity(Data, _Env, #occi_link{}=Link) ->
    case parse_full(Data, #state{entity=Link}) of
        {error, Reason} ->
            {error, {parse_error, Reason}};
        {ok, #occi_link{}=Link2} ->
            {ok, Link2};
        _ ->
            {error, {parse_error, not_an_entity}}
    end;

parse_entity(Data, _Env, #occi_entity{id=Id}) ->
    case parse_full(Data, #state{entity_id=Id}) of
        {error, Reason} ->
            {error, {parse_error, Reason}};
        {ok, #occi_resource{}=Res2} ->
            {ok, Res2};
        {ok, #occi_link{}=Link2} ->
            {ok, Link2};
        _ ->
            {error, {parse_error, not_an_entity}}
    end.

parse_user_mixin(Data, _Env) ->
    case parse_full(Data, #state{mixin=occi_mixin:new(#occi_cid{class=mixin})}) of
        {error, Reason} ->
            {error, {parse_error, Reason}};
        {ok, #occi_mixin{}=Mixin} ->
            {ok, Mixin};
        Err ->
            ?error("Invalid request: ~p~n", [Err]),
            {error, {parse_error, Err}}
    end.

parse_collection(Data, _Env) ->
    case parse_full(Data) of
        {error, Reason} ->
            {error, {parse_error, Reason}};
        {ok, #occi_collection{}=Coll} ->
            {ok, Coll}
    end.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
parse_extension(Data, _Env) ->
    case parse_full(Data, #state{}) of
        {ok, #occi_extension{}=Ext} ->
            ?info("Loaded extension: ~s~n", [occi_extension:get_name(Ext)]),
            Ext;
        {error, Reason} ->
            ?error("Error loading extension: ~p~n", [Reason]),
            {error, parse_error};
        _ ->
            {error, {parse_error, not_an_extension}}
    end.

init(#parser{}=Parser) ->
    {ok, init, Parser#parser{stack=[init]}}.

init(E=?extension, _From, #parser{state=State}=Ctx) ->
    Ext = make_extension(E, State),
    push(extension, 
         ?set_state(Ctx, State#state{extension=Ext, prefixes=load_prefixes(E#xmlel.declared_ns)}));

init(E=?resource, _From, #parser{state=State}=Ctx) ->
    try make_resource(E, State) of
        #state{}=State2 ->
            push(resource, ?set_state(Ctx, State2#state{prefixes=load_prefixes(E#xmlel.declared_ns)}));
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    catch
        _:Err ->
            {reply, {error, Err}, eof, Ctx}
    end;

init(E=?link, _From, #parser{state=State}=Ctx) ->
    try make_link(E, State) of
        #state{}=State2 ->
            push(link, ?set_state(Ctx, State2#state{prefixes=load_prefixes(E#xmlel.declared_ns)}));
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    catch
        _:Err ->
            {reply, {error, Err}, eof, Ctx}
    end;

init(E=?collection, _From, #parser{state=State}=Ctx) ->
    try make_collection(E, State) of
        #state{}=State2 ->
            push(collection, ?set_state(Ctx, State2#state{prefixes=load_prefixes(E#xmlel.declared_ns)}));
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    catch
        _:Err ->
            {reply, {error, Err}, eof, Ctx}
    end;

init(E=?mixin, _From, #parser{state=State}=Ctx) ->
    try make_mixin(E, State) of
        #occi_mixin{}=Mixin ->
            push(mixin, ?set_state(Ctx, State#state{mixin=Mixin}))
    catch
        _:Err ->
            {reply, {error, Err}, eof, Ctx}
    end;

init(E=?action, _From, #parser{state=State}=Ctx) ->
    try make_action(E, State) of
        #state{}=State2 ->
            push(action, ?set_state(Ctx, State2));
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    catch
        _:Err ->
            {reply, {error, Err}, eof, Ctx}
    end;

init(E, _From, Ctx) ->
    other_event(E, Ctx, init).

action(E=?attribute, _From, 
       #parser{state=#state{action=Action}=State}=Ctx) ->
    case make_attribute(E, State) of
        {ok, Key, Val} ->
            try occi_action:set_attr_value(Action, Key, Val) of
                #occi_action{}=Action2 ->
                    {reply, ok, action,
                     ?set_state(Ctx, State#state{action=Action2})}
            catch
                throw:Err ->
                    {reply, {error, Err}, eof, Ctx}
            end;
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    end;
action(_E=?attributeEnd, _From, Ctx) ->
    {reply, ok, action, Ctx};
action(_E=?actionEnd, _From, 
       #parser{state=#state{request=_Req, action=Action}}=Ctx) ->
    {reply, {eof, Action}, eof, Ctx};
action(E, _From, Ctx) ->
    other_event(E, Ctx, action).

collection(E=?entity, _From, #parser{state=#state{collection=Coll}=State}=Ctx) ->
    case erim_xml:get_attribute_node(E, ?xlink_ns, <<"href">>) of
        undefined ->
            {reply, {error, invalid_entity}, eof, Ctx};
        #xmlattr{value=Val} ->
            try occi_uri:parse(Val) of
                #uri{}=Uri ->
                    {reply, ok, collection,
                     ?set_state(Ctx, State#state{collection=occi_collection:add_entity(Coll, Uri)})}
            catch
                _:Err ->
                    {reply, {error, Err}, eof, Ctx}
            end
    end;
collection(_E=?entityEnd, _From, Ctx) ->
    {reply, ok, collection, Ctx};
collection(_E=?collectionEnd, _From,
           #parser{state=#state{request=_Req, collection=Coll}}=Ctx) ->
    {reply, {eof, Coll}, eof, Ctx};
collection(E, _From, Ctx) ->
    other_event(E, Ctx, collection).

resource(E=?kind, _From, 
         #parser{state=#state{entity=#occi_resource{cid=undefined}=Res}=State}=Ctx) ->
    case get_cid(E) of
        #occi_cid{}=Cid ->
            C2 = Cid#occi_cid{class=kind},
            case occi_store:get(C2) of
                {ok, #occi_kind{parent=#occi_cid{term=resource}}=Kind} ->
                    Res2 = occi_resource:set_cid(Res, Kind),
                    {reply, ok, resource, ?set_state(Ctx, State#state{entity=Res2})};
                _ ->
                    {reply, {error, {invalid_cid, C2}}, eof, Ctx}
            end;
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    end;

resource(E=?kind, _From, 
         #parser{state=#state{entity=#occi_resource{cid=#occi_cid{scheme=Scheme, 
                                                                  term=Term, 
                                                                  class=kind}}}}=Ctx) ->
    case get_cid(E) of
        #occi_cid{scheme=Scheme, term=Term} ->
            {reply, ok, resource, Ctx};
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    end;

resource(E=?link, _From, #parser{state=#state{}=S}=Ctx) ->
    try make_link(E, S#state{link=occi_link:new()}) of
        #state{}=S2 -> push(link, ?set_state(Ctx, S2));
        {error, Err} -> {reply, {error, Err}, eof, Ctx}
    catch
        throw:Err -> {reply, {error, Err}, eof, Ctx}
    end;

resource(?kindEnd, _From, Ctx) ->
    {reply, ok, resource, Ctx};
resource(E=?mixin, _From, 
         #parser{state=#state{entity=Res}=State}=Ctx) ->
    case get_cid(E) of
        #occi_cid{}=Cid  ->
            C2 = Cid#occi_cid{class=mixin},
            case occi_store:get(C2) of
                {ok, #occi_mixin{}=Mixin} ->
                    Res2 = occi_resource:add_mixin(Res, Mixin),
                    {reply, ok, resource, 
                     ?set_state(Ctx, State#state{entity=Res2})};
                _ ->
                    {reply, {error, {invalid_cid, C2}}, eof, Ctx}
            end;
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    end;

resource(?mixinEnd, _From, Ctx) ->
    {reply, ok, resource, Ctx};

resource(E=?attribute, _From, 
         #parser{state=#state{entity=#occi_resource{}=Res}=State}=Ctx) ->
    case make_attribute(E, State) of
        {ok, Key, Val} ->
            try occi_resource:set_attr_value(Res, Key, Val) of
                #occi_resource{}=Res2 ->
                    {reply, ok, resource,
                     ?set_state(Ctx, State#state{entity=Res2})}
            catch
                _:Err ->
                    {reply, {error, Err}, eof, Ctx}
            end;
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    end;	

resource(?attributeEnd, _From, Ctx) ->
    {reply, ok, resource, Ctx};

resource(?resourceEnd, _From, #parser{state=#state{request=_Req, entity=Res}}=Ctx) ->
    {reply, {eof, Res}, eof, Ctx};

resource(E, _From, Ctx) ->
    other_event(E, Ctx, resource).

link(E=?kind, _From, 
     #parser{state=#state{entity=#occi_resource{}, link=#occi_link{}=L}=S}=Ctx) ->
    case get_cid(E) of
        #occi_cid{}=Cid ->
            C2 = Cid#occi_cid{class=kind},
            case occi_store:get(C2) of
                {ok, #occi_kind{parent=#occi_cid{term=link}}=Kind} ->
                    L2 = occi_link:set_cid(L, Kind),
                    {reply, ok, link, ?set_state(Ctx, S#state{link=L2})};
                _ ->
                    {reply, {error, {invalid_cid, C2}}, eof, Ctx}
            end;
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    end;

link(E=?kind, _From, #parser{state=#state{entity=#occi_link{cid=undefined}=L}=S}=Ctx) ->
    case get_cid(E) of
        #occi_cid{}=Cid ->
            C2 = Cid#occi_cid{class=kind},
            case occi_store:get(C2) of
                {ok, #occi_kind{parent=#occi_cid{term=link}}=Kind} ->
                    L2 = occi_link:set_cid(L, Kind),
                    {reply, ok, link, ?set_state(Ctx, S#state{entity=L2})};
                _ ->
                    {reply, {error, {invalid_cid, C2}}, eof, Ctx}
            end;
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    end;

link(E=?kind, _From, 
     #parser{state=#state{entity=#occi_link{cid=#occi_cid{scheme=Scheme, term=Term, class=kind}}}}=Ctx) ->
    case get_cid(E) of
        #occi_cid{scheme=Scheme, term=Term} ->
            {reply, ok, link, Ctx};
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    end;

link(?kindEnd, _From, Ctx) ->
    {reply, ok, link, Ctx};

link(E=?mixin, _From, #parser{state=#state{entity=#occi_resource{}, link=L}=S}=Ctx) ->
    case get_cid(E) of
        #occi_cid{}=Cid ->
            C2 = Cid#occi_cid{class=mixin},
            case occi_store:get(C2) of
                {ok, #occi_mixin{}=Mixin} ->
                    L2 = occi_link:add_mixin(L, Mixin),
                    {reply, ok, link, ?set_state(Ctx, S#state{link=L2})};
                _ ->
                    {reply, {error, {invalid_cid, C2}}, eof, Ctx}
            end;
        {error, Err} -> {reply, {error, Err}, eof, Ctx}
    end;

link(E=?mixin, _From, #parser{state=#state{entity=L}=S}=Ctx) ->
    case get_cid(E) of
        #occi_cid{}=Cid ->
            C2 = Cid#occi_cid{class=mixin},
            case occi_store:get(C2) of
                {ok, #occi_mixin{}=Mixin} ->
                    L2 = occi_link:add_mixin(L, Mixin),
                    {reply, ok, link, ?set_state(Ctx, S#state{entity=L2})};
                _ ->
                    {reply, {error, {invalid_cid, C2}}, eof, Ctx}
            end;
        {error, Err} -> {reply, {error, Err}, eof, Ctx}
    end;

link(?mixinEnd, _From, Ctx) ->
    {reply, ok, link, Ctx};

link(E=?attribute, _From, #parser{state=#state{entity=#occi_resource{}, link=#occi_link{}=L}=S}=Ctx) ->
    case make_attribute(E, S) of
        {ok, 'occi.core.source', _} ->
            % Inline link: source is enclosing resource
            {reply, {error, {invalid_attribute, 'occi.core.source'}}, eof, Ctx};
        {ok, Key, Val} ->
            try occi_link:set_attr_value(L, Key, Val) of
                #occi_link{}=L2 ->
                    {reply, ok, link, ?set_state(Ctx, S#state{link=L2})}
            catch throw:Err -> {reply, {error, Err}, eof, Ctx}
            end;
        {error, Err} -> {reply, {error, Err}, eof, Ctx}
    end;	

link(E=?attribute, _From, #parser{state=#state{entity=#occi_link{}=L}=S}=Ctx) ->
    case make_attribute(E, S) of
        {ok, Key, Val} ->
            try occi_link:set_attr_value(L, Key, Val) of
                #occi_link{}=L2 ->
                    {reply, ok, link, ?set_state(Ctx, S#state{entity=L2})}
            catch throw:Err -> {reply, {error, Err}, eof, Ctx}
            end;
        {error, Err} -> {reply, {error, Err}, eof, Ctx}
    end;	

link(?attributeEnd, _From, Ctx) ->
    {reply, ok, link, Ctx};

link(?linkEnd, _From, #parser{state=#state{entity=#occi_resource{}=R, link=#occi_link{}=L}=S}=Ctx) ->
    pop(?set_state(Ctx, S#state{entity=occi_resource:add_link(R, L), link=undefined}));

link(?linkEnd, _From, #parser{state=#state{request=_Req, entity=Link}}=Ctx) ->
    {reply, {eof, Link}, eof, Ctx};

link(E, _From, Ctx) ->
    other_event(E, Ctx, link).

extension(E=?kind, _From, #parser{state=State}=Ctx) ->
    Kind = make_kind(E, State),
    push(kind, ?set_state(Ctx, State#state{kind=Kind}));
extension(E=?mixin, _From, #parser{state=State}=Ctx) ->
    Mixin = make_mixin(E, State),
    push(mixin, ?set_state(Ctx, State#state{mixin=Mixin}));
extension(?extensionEnd, _From, #parser{state=#state{extension=Ext}=State}=Ctx) ->
    {reply, {eof, Ext}, eof, ?set_state(Ctx, State#state{extension=undefined})};
extension(E, _From, Ctx) ->
    other_event(E, Ctx, extension).

kind(E=?parent, _From, #parser{state=#state{kind=Kind}=State}=Ctx) ->
    case get_cid(E) of
        #occi_cid{}=Cid ->
            {reply, ok, kind, 
             ?set_state(Ctx, State#state{kind=occi_kind:set_parent(Kind, Cid)})};
        {error, Reason} ->
            {reply, {error, Reason}, eof, Ctx}
    end;
kind(_E=?parentEnd, _From, Ctx) ->
    {reply, ok, kind, Ctx};
kind(E=?attribute, _From, #parser{state=State}=Ctx) ->
    try make_attr_spec(E, State) of
        AttrSpec -> 
            push(attribute_spec, ?set_state(Ctx, State#state{attribute=AttrSpec}))
    catch
        _:Err ->
            {reply, {error, Err}, eof, Ctx}
    end;
kind(E=?action, _From, #parser{state=State}=Ctx) ->
    try make_action_spec(E, State) of
        #occi_action{}=Action ->
            push(action_spec, 
                 ?set_state(Ctx, State#state{action=Action}));
        {error, Reason} ->
            {reply, {error, Reason}, eof, Ctx}
    catch
        _:Err ->
            {reply, {error, Err}, eof, Ctx}
    end;
kind(_E=?kindEnd, _From, #parser{state=#state{extension=Ext, kind=Kind}=State}=Ctx) ->
    pop(?set_state(Ctx, State#state{kind=undefined, 
                                    extension=occi_extension:add_kind(Ext, Kind)}));
kind(E, _From, Ctx) ->
    other_event(E, Ctx, kind).

mixin(E=?depends, _From, #parser{state=#state{mixin=Mixin}=State}=Ctx) ->
    case get_cid(E) of
        #occi_cid{}=Cid->
            case check_cid(Cid, State) of
                #occi_cid{}=C2 ->
                    Mixin2 = occi_mixin:add_depends(Mixin, C2),
                    {reply, ok, mixin, ?set_state(Ctx, State#state{mixin=Mixin2})};
                {error, Err} ->
                    {reply, {error, Err}, eof, Ctx}
            end;
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    end;

mixin(_E=?dependsEnd, _From, Ctx) ->
    {reply, ok, mixin, Ctx};

mixin(E=?applies, _From, #parser{state=#state{mixin=Mixin}=State}=Ctx) ->
    case get_cid(E) of
        #occi_cid{}=Cid ->
            case check_cid(Cid, State) of
                #occi_cid{}=C2 ->
                    Mixin2 = occi_mixin:add_applies(Mixin, C2),
                    {reply, ok, mixin, ?set_state(Ctx, State#state{mixin=Mixin2})};
                {error, Err} ->
                    {reply, {error, Err}, eof, Ctx}
            end;
        {error, Err} ->
            {reply, {error, Err}, eof, Ctx}
    end;

mixin(_E=?appliesEnd, _From, Ctx) ->
    {reply, ok, mixin, Ctx};

mixin(E=?attribute, _From, #parser{state=State}=Ctx) ->
    try make_attr_spec(E, State) of
        Attr -> 
            push(attribute_spec, ?set_state(Ctx, State#state{attribute=Attr}))
    catch
        _:Err ->
            {reply, {error, Err}, eof, Ctx}
    end;

mixin(E=?action, _From, #parser{state=State}=Ctx) ->
    try make_action_spec(E, State) of
        #occi_action{}=Action ->
            push(action_spec, ?set_state(Ctx, State#state{action=Action}));
        {error, Reason} ->
            {reply, {error, Reason}, eof, Ctx}
    catch
        _:Err ->
            {reply, {error, Err}, eof, Ctx}
    end;

mixin(_E=?mixinEnd, _From, 
      #parser{state=#state{extension=undefined, request=_Req, mixin=Mixin}}=Ctx) ->
    case occi_mixin:is_valid(Mixin) of
        true ->
            {reply, {eof, Mixin}, eof, Ctx};
        {false, Err} ->
            {reply, {error, Err}, eof, Ctx}
    end;

mixin(_E=?mixinEnd, _From, #parser{state=#state{extension=Ext, mixin=Mixin}=State}=Ctx) ->
    pop(?set_state(Ctx, State#state{mixin=undefined,
                                    extension=occi_extension:add_mixin(Ext, Mixin)}));

mixin(E, _From, Ctx) ->
    other_event(E, Ctx, mixin).

attribute_spec(_E=?attributeEnd, _From, 
               #parser{stack=[_Cur,Prev|_Stack], state=#state{attribute=A}=State}=Ctx) ->
    State2 = case Prev of
                 kind -> 
                     Ref = State#state.kind,
                     State#state{kind=occi_kind:add_attribute(Ref, A)};
                 mixin -> 
                     Ref = State#state.mixin,
                     State#state{mixin=occi_mixin:add_attribute(Ref, A)};
                 action_spec -> 
                     Ref = State#state.action,
                     State#state{action=occi_action:add_attribute(Ref, A)}
             end,
    pop(?set_state(Ctx, State2#state{attribute=undefined}));
attribute_spec(E, _From, Ctx) ->
    other_event(E, Ctx, attribute_spec).

action_spec(E=?attribute, _From, #parser{state=State}=Ctx) ->
    try make_attr_spec(E, State) of
        AttrSpec -> 
            push(attribute_spec, ?set_state(Ctx, State#state{attribute=AttrSpec}))
    catch
        _:Err ->
            {reply, {error, Err}, eof, Ctx}
    end;
action_spec(_E=?actionEnd, _From, 
            #parser{stack=[_Cur,Prev|_Stack], state=#state{action=A}=State}=Ctx) ->
    State2 = case Prev of
                 kind -> 
                     State#state{kind=occi_kind:add_action(State#state.kind, A)};
                 mixin -> 
                     State#state{mixin=occi_mixin:add_action(State#state.mixin, A)}
             end,
    pop(?set_state(Ctx, State2#state{action=undefined}));
action_spec(E, _From, Ctx) ->
    other_event(E, Ctx, action_spec).

eof(_Event, _From, State) ->
    {reply, ok, eof, State}.

%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%-------------------------------------------------------------------
handle_event(stop, _, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_full(#xmlel{}=El, Ctx) ->
    P = #parser{state=Ctx},
    case gen_fsm:start(?MODULE, P, []) of
        {ok, Ref} ->
            P0 = P#parser{id=Ref},
            Res = parse(P0, El),
            stop(P0),
            Res;
        Err ->
            ?error("Error starting xml parser: ~p~n", [Err]),
            {error, parser_error}
    end;
parse_full(<<>>, _) ->
    {error, invalid_request};
parse_full(Data, Ctx) ->
    P = start(Ctx),
    Res = parse(P, Data),
    stop(P),
    Res.

-spec parse(parser(), xmlel() | binary()) -> parser_result().
parse(#parser{}=P, #xmlel{}=El) ->
    Evts = lists:flatten(to_events(El)),
    send_events(#parser{sink=P}, Evts);
parse(#parser{src=#parser{id=Src}}=P, Data) ->
    Parser0 = #parser{sink=P},
    try erim_xml:parse_final(Src, Data) of
        done ->
            send_events(Parser0, []);
        Events ->
            send_events(Parser0, Events)
    catch
        Err -> {error, Err}
    end.

to_events(#xmlel{ns=NS, name=Name, children=undefined}=El) ->
    [El, #xmlendtag{ns=NS, name=Name}];
to_events(#xmlel{ns=NS, name=Name, children=Children}=El) ->
    [El#xmlel{children=undefined}, 
     lists:map(fun (#xmlel{}=Child) -> to_events(Child) end, Children),
     #xmlendtag{ns=NS, name=Name}].

start(Ctx) ->
    Parser = #parser{state=Ctx},
    case gen_fsm:start(?MODULE, Parser, []) of
        {ok, Ref} ->
            Src = #parser{id=erim_xml:start_parser(?PARSER_OPTIONS)},
            Parser#parser{id=Ref, src=Src};
        Err -> 
            ?error("Error starting xml parser: ~p~n", [Err]),
            throw({error, parser_error})
    end.

stop(#parser{id=Ref, src=undefined}) ->
    gen_fsm:send_all_state_event(Ref, stop);
stop(#parser{id=Ref, src=#parser{id=Src}}) ->
    erim_xml:stop_parser(Src),
    gen_fsm:send_all_state_event(Ref, stop).

send_events(_P, []) ->
    {error, {parse_error, incomplete}};
send_events(#parser{}=P, [E|T]) ->
    case occi_parser:send_event(E, ok, P) of
        {reply, {error, Err}, _, _} ->
            {error, Err};
        ok ->
            send_events(P, T);
        {reply, {eof, Result}, _, _} ->
            {ok, Result}
    end.

get_attr_value(XmlEl, Name) ->
    case get_attr_value(XmlEl, Name, undefined) of
        undefined ->
            throw({error, {missing_attribute, Name}});
        Val -> Val
    end.

get_attr_value(#xmlel{}=XmlEl, Name, Default) ->
    case erim_xml:get_attribute_node(XmlEl, Name) of
        undefined -> Default;
        Attr -> Attr#xmlattr.value
    end.

push(Next, #parser{stack=undefined}=Ctx) ->
    push(Next, Ctx#parser{stack=[]});
push(Next, #parser{stack=Stack}=Ctx) ->
    {reply, ok, Next, Ctx#parser{stack=[Next|Stack]}}.

pop(#parser{stack=[]}=Ctx) ->
    {reply, {error, stack_error}, eof, Ctx};
pop(#parser{stack=[_Cur,Previous|Stack]}=Ctx) ->
    {reply, ok, Previous, Ctx#parser{stack=[Previous|Stack]}}.

make_extension(E, _State) ->
    Name = get_attr_value(E, <<"name">>, undefined),
    Version = get_attr_value(E, <<"version">>, undefined),
    ?info("Load extension: ~s v~s~n", [Name, Version]),
    occi_extension:new(Name, Version).

make_resource(E, #state{entity=undefined}=State) ->
    make_resource_title(E, State#state{entity=occi_resource:new()});
make_resource(E, #state{entity=#occi_resource{}}=State) ->
    make_resource_title(E, State).

make_resource_title(E, #state{entity=Res}=State) ->
    case erim_xml:get_attribute(E, <<"title">>, undefined) of
        undefined ->
            make_resource_id(E, State);
        Title ->
            make_resource_id(E, State#state{entity=occi_resource:set_attr_value(Res, 'occi.core.title', Title)})
    end.

make_resource_id(E, #state{entity_id=undefined, entity=#occi_resource{id=undefined}=Res}=State) ->
    case get_attr_value(E, <<"id">>, undefined) of
        undefined ->
            State;
        Id ->
            State#state{entity=occi_resource:set_id(Res, occi_uri:parse(Id))}
    end;
make_resource_id(E, #state{entity_id=undefined, entity=#occi_resource{id=Uri}}=S) ->
    case get_attr_value(E, <<"id">>, undefined) of
        undefined -> S;
        Id ->
            try occi_uri:parse(Id) of
                #uri{}=Uri -> S;
                O -> {error, {invalid_id, O}}
            catch throw:Err -> {error, Err}
            end
    end;
make_resource_id(E, #state{entity_id=Uri, entity=Res}=S) ->
    case get_attr_value(E, <<"id">>, undefined) of
        undefined -> S#state{entity_id=undefined, entity=occi_resource:set_id(Res, Uri)};
        Id ->
            try occi_uri:parse(Id) of
                #uri{}=Uri -> S#state{entity_id=undefined, entity=occi_resource:set_id(Res, Uri)};
                O -> {error, {invalid_id, O}}
            catch throw:Err -> {error, Err}
            end
    end.

make_link(E, #state{entity=undefined}=State) ->
    make_link_title(E, State#state{entity=occi_link:new()});
make_link(E, #state{}=State) ->
    make_link_title(E, State).

make_link_title(E, #state{entity=Link}=State) ->
    case erim_xml:get_attribute(E, <<"title">>, undefined) of
        undefined ->
            make_link_id(E, State);
        Title ->
            make_link_id(E, State#state{entity=occi_link:set_attr_value(Link, 'occi.core.title', Title)})
    end.

make_link_id(E, #state{entity=#occi_resource{}, link=#occi_link{}=L}=S) ->
    case get_attr_value(E, <<"id">>, undefined) of
        undefined -> S;
        Id ->  S#state{link=occi_link:set_id(L, occi_uri:parse(Id))}
    end;

make_link_id(E, #state{entity_id=undefined, entity=#occi_link{id=undefined}=L}=S) ->
    case get_attr_value(E, <<"id">>, undefined) of
        undefined -> S;
        Id -> S#state{entity=occi_link:set_id(L, occi_uri:parse(Id))}
    end;

make_link_id(E, #state{entity_id=undefined, entity=#occi_link{id=Uri}}=S) ->
    case get_attr_value(E, <<"id">>, undefined) of
        undefined -> S;
        Id ->
            try occi_uri:parse(Id) of
                Uri -> S;
                O -> {error, {invalid_id, O}}
            catch throw:Err -> 
		    ?debug("Error parsing URI ~p: ~p~n", [Id, Err]),
		    {error, Err}
            end
    end;

make_link_id(E, #state{entity_id=Uri, entity=Link}=State) ->
    case get_attr_value(E, <<"id">>, undefined) of
        undefined ->
            State#state{entity_id=undefined, entity=occi_link:set_id(Link, Uri)};
        Id ->
            try occi_uri:parse(Id) of
                #uri{}=Uri -> State#state{entity_id=undefined, entity=occi_link:set_id(Link, Uri)};
                O -> {error, {invalid_id, O}}
            catch throw:Err -> 
		    ?debug("Error parsing URI ~p: ~p~n", [Id, Err]),
		    {error, Err}
            end
    end.

make_kind(E, #state{extension=Ext}) ->
    Scheme = ?scheme_to_atom(get_attr_value(E, <<"scheme">>, Ext#occi_extension.scheme)),
    Term = ?term_to_atom(get_attr_value(E, <<"term">>)),
    ?info("Load kind: ~s~s~n", [Scheme, Term]),
    Kind = occi_kind:new(Scheme, Term),
    Title = get_attr_value(E, <<"title">>, undefined),
    occi_kind:set_title(Kind, Title).

make_mixin(E, #state{extension=#occi_extension{}=Ext}) ->
    Scheme = ?scheme_to_atom(get_attr_value(E, <<"scheme">>, Ext#occi_extension.scheme)),
    Term = ?term_to_atom(get_attr_value(E, <<"term">>)),
    ?info("Load mixin: ~s~s~n", [Scheme, Term]),
    Mixin = occi_mixin:new(Scheme, Term),
    Title = get_attr_value(E, <<"title">>, undefined),
    occi_mixin:set_title(Mixin, Title);

make_mixin(E, _S) ->
    Id = #occi_cid{scheme=?scheme_to_atom(get_attr_value(E, <<"scheme">>)), 
                   term=?term_to_atom(get_attr_value(E, <<"term">>)),
                   class=mixin},
    occi_mixin:set_title(
      occi_mixin:set_location(
        occi_mixin:new(Id),
        get_attr_value(E, <<"location">>)), 
      get_attr_value(E, <<"title">>, undefined)).    

make_collection(_E, #state{}=State) ->
    State#state{collection=occi_collection:new()}.

make_attr_spec(E, State) ->
    Name = get_attr_value(E, <<"name">>),
    Type = get_type_id(E, <<"type">>, State),
    ?debug("Load attribute spec: ~s~n", [Name]),
    Attr = occi_attribute:new(Name),
    Attr2 = occi_attribute:set_type(Attr, Type),
    Attr3 = case get_attr_value(E, <<"default">>, undefined) of
                undefined -> Attr2;
                Default -> Def = binary_to_atom(Default, latin1),
                           occi_attribute:set_default(Attr2, Def)
            end,
    Attr4 = case get_attr_value(E, <<"use">>, undefined) of
                undefined -> Attr3;
                _ -> occi_attribute:set_required(Attr3, true)
            end,
    Attr5 = case get_attr_value(E, <<"immutable">>, undefined) of
                undefined -> Attr4;
                _ -> occi_attribute:set_immutable(Attr4, true)
            end,
    Title = get_attr_value(E, <<"title">>, undefined),
    occi_attribute:set_title(Attr5, Title).

make_attribute(E, _State) ->
    case get_attr_value(E, <<"name">>, undefined) of
        undefined ->
            {error, invalid_attribute};
        Name ->
            case erim_xml:get_attribute_node(E, <<"value">>) of
                undefined ->
                    case erim_xml:get_attribute_node(E, ?xlink_ns, <<"href">>) of
                        undefined ->
                            {error, invalid_attribute};
                        #xmlattr{value=Val} ->
                            try occi_uri:parse(Val) of
                                #uri{}=Uri ->
                                    {ok, ?attr_to_atom(Name), Uri}
                            catch
                                throw:Err ->
                                    {error, Err}
                            end
                    end;
                #xmlattr{value=Val} ->
                    {ok, ?attr_to_atom(Name), Val}
            end
    end.

make_action(E, #state{action=#occi_action{id=#occi_cid{term=Term}}}=State) ->
    case get_cid(E) of
        #occi_cid{term=Term}=Cid ->
            C2 = Cid#occi_cid{class=action},
            case occi_store:get(C2) of
                {ok, #occi_action{}=Action} ->
                    State#state{action=Action};
                _ ->
                    {error, {invalid_cid, C2}}
            end;
        {error, Err} -> {error, Err}
    end;

make_action(E, State) ->
    case get_cid(E) of
        #occi_cid{}=Cid ->
            C2 = Cid#occi_cid{class=action},
            case occi_store:get(C2) of
                {ok, #occi_action{}=Action} ->
                    State#state{action=Action};
                _ ->
                    {error, {invalid_cid, C2}}
            end;
        {error, Err} -> {error, Err}
    end.

make_action_spec(E, _State) ->
    case get_cid(E) of
        #occi_cid{}=Cid ->
            C2 = Cid#occi_cid{class=action},
            ?debug("Load action spec: ~p~n", [C2]),
            Action = occi_action:new(C2),
            Title = get_attr_value(E, <<"title">>, undefined),
            occi_action:set_title(Action, Title);
        {error, Err} ->
            {error, Err}
    end.

get_cid(E) ->
    case get_attr_value(E, <<"scheme">>, undefined) of
        undefined ->
            {error, {invalid_cid, missing_scheme}};
        Scheme ->
            case get_attr_value(E, <<"term">>, undefined) of
                undefined ->
                    {error, {invalid_cid, missing_term}};
                Term ->
                    #occi_cid{scheme=?scheme_to_atom(Scheme), term=?term_to_atom(Term), class='_'}
            end
    end.

check_cid(#occi_cid{}=Cid, #state{extension=Ext}) ->
    case occi_extension:find(Ext, Cid) of
        [#occi_kind{id=Cid2}] -> Cid2;
        [#occi_mixin{id=Cid2}] -> Cid2;
        [] ->
            case occi_store:get(Cid) of
                {ok, #occi_kind{id=Cid2}} -> Cid2;
                {ok, #occi_mixin{id=Cid2}} -> Cid2;
                _ -> {error, {invalid_cid, Cid}}
            end
    end.

% Return a dict with prefix->ns_as_atom key/value
-spec load_prefixes([{xmlname(), string()}]) -> term().
load_prefixes(NS) ->
    load_prefixes(NS, []).

load_prefixes([], Acc) ->
    dict:from_list(Acc);
load_prefixes([{Name, Prefix}|Tail], Acc) ->
    load_prefixes(Tail, [{Prefix, Name}|Acc]).

get_type_id(#xmlel{}=E, Name, #state{prefixes=P}) ->
    case get_attr_value(E, Name, undefined) of
        undefined -> undefined;
        Bin -> resolve_ns(Bin, P)
    end.

resolve_ns(Bin, Dict) ->
    case string:tokens(binary_to_list(Bin), ":") of
        [Ns, Name] ->
            case dict:find(Ns, Dict) of
                error ->
                    throw({error, {parse_error, unknown_ns}});
                {ok, Val} ->
                    {Val, list_to_atom(Name)}
            end;
        [_Name] ->
            throw({error, {parse_error, unknown_ns}})
    end.

other_event(E, Ctx, StateName) ->
    case is_ws(E) of
        true ->
            {reply, ok, StateName, Ctx};
        false ->
            parse_error(E, Ctx)
    end.

is_ws(#xmlcdata{cdata = <<  "\n", Bin/binary >>}=E) ->
    is_ws(E#xmlcdata{cdata=Bin});
is_ws(#xmlcdata{cdata = <<  "\t", Bin/binary >>}=E) ->
    is_ws(E#xmlcdata{cdata=Bin});
is_ws(#xmlcdata{cdata = <<  " ", Bin/binary >>}=E) ->
    is_ws(E#xmlcdata{cdata=Bin});
is_ws(#xmlcdata{cdata = <<>>}) ->
    true;
is_ws(_) ->
    false.

parse_error(E, Ctx) ->
    ?error(build_err(E)),
    {reply, {error, parse_error}, eof, Ctx}.

build_err(#xmlel{name=Name}) ->
    io_lib:format("Invalid element: ~p", [Name]);
build_err(#xmlendtag{name=Name}) ->
    io_lib:format("Invalid element: ~p", [Name]);
build_err(E) ->
    io_lib:format("Invalid element: ~p", [E]).
