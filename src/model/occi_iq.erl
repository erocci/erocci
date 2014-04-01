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
%%% @doc See schemas/occi-xmpp.xsd for OCCI IQ schema
%%%
%%% @end
%%% Created : 26 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_iq).
-compile({parse_transform, lager_transform}).

-include("occi.hrl").
-include("occi_xml.hrl").
-include("occi_xmpp.hrl").
-include_lib("erim/include/exmpp.hrl").
-include_lib("erim/include/exmpp_jid.hrl").

-export([q/2,
	 result/1,
	 result/2,
	 error/2]).

-export([is_error/1,
	 to_record/1]).

-export([has_node/1,
	 get_node/1,
	 get_type/1,
	 get_op/1,
	 get_payload/1,
	 set_node_attr/2]).

-spec q(To :: #jid{}, Node :: uri() | binary()) -> xmlel().
q(#jid{raw=To}, #uri{path=Path}) ->
    q(To, Path);

q(To, Path) when is_binary(Path) ->
    Query = exmpp_xml:element(?occi_ns, 'query', [exmpp_xml:attribute(<<"node">>, Path)], []),
    Iq = ?IQ_GET(To, iq_id()),
    exmpp_xml:append_child(Iq, Query).

-spec result(Iq :: #xmlel{} | #occi_iq{}) -> #xmlel{}.
result(#xmlel{children=[Q]}=Iq) ->
    exmpp_xml:append_child(exmpp_iq:result(Iq), Q);
result(#occi_iq{raw=Raw}=Iq) ->
    Iq#occi_iq{raw=result(Raw)};
result(_) ->
    throw({error, invalid_occi_iq}).

-spec result(Iq :: #xmlel{} | #occi_iq{}, #xmlel{}) -> #xmlel{}.
result(#xmlel{children=[Q]}=Iq, #xmlel{}=Res) ->
    Q2 = Q#xmlel{children=[Res]},
    exmpp_xml:append_child(exmpp_iq:result(Iq), Q2);
result(#occi_iq{raw=Raw}=Iq, #xmlel{}=Res) ->
    Iq#occi_iq{raw=result(Raw, Res)};
result(_, _) ->
    throw({error, invalid_occi_iq}).

-spec error(xmlel() | occi_iq(), xmlel() | atom()) -> xmlel() | occi_iq().
error(#xmlel{}=Iq, Condition) ->
    exmpp_iq:error(Iq, Condition);
error(#occi_iq{raw=Raw}=Iq, Condition) ->
    Raw2 = exmpp_iq:error(Raw, Condition),
    Iq#occi_iq{raw=Raw2}.

-spec to_record(xmlel()) -> occi_iq().
to_record(#xmlel{children=[_El]}=Iq) ->
    #occi_iq{raw=Iq,
	     op=get_op(Iq),
	     node=get_node(Iq),
	     type=get_type(Iq)
	    };
to_record(_) ->
    throw({error, invalid_occi_iq}).

-spec is_error(#xmlel{} | occi_iq()) -> boolean().
is_error(#xmlel{}=Iq) ->
    exmpp_iq:is_error(Iq);
is_error(#occi_iq{raw=Iq}) ->
    exmpp_iq:is_error(Iq).

-spec has_node(xmlel()) -> boolean().
has_node(#xmlel{children=[El]}) ->
    case exmpp_xml:get_attribute(El, <<"node">>, undefined) of
	undefined -> false;
	_ -> true
    end;
has_node(_) ->
    false.

-spec get_node(xmlel()) -> occi_node().
get_node(#xmlel{children=[El]}=Iq) ->
    case get_type(Iq) of
	occi_query ->
	    case exmpp_xml:get_attribute(El, <<"node">>, undefined) of
		undefined ->
		    #occi_node{type=occi_query, data=undefined, _='_'};
		Cid ->
		    #occi_node{type=occi_user_mixin, objid=occi_cid:parse(Cid), _='_'}
	    end;
        occi_collection ->
	    Cid = occi_cid:parse(exmpp_xml:get_attribute(El, <<"node">>, undefined)),
	    #occi_node{type=occi_collection, objid=Cid, _='_'};
	occi_entity ->
	    case exmpp_xml:get_attribute(El, <<"node">>, undefined) of
		undefined ->
		    #occi_node{id=undefined, type=undefined, objid=undefined};
		S ->
		    Id = occi_uri:parse(S),
		    #occi_node{id=#uri{path=Id#uri.path}, objid=Id, type=undefined, _='_'}
	    end
    end;
get_node(_El) ->
    lager:error("Invalid OCCI IQ: ~p~n", [lager:pr(_El, ?MODULE)]),
    throw({error, invalid_occi_iq}).

-spec get_type(xmlel()) -> occi_entity | occi_query | occi_collection.
get_type(#xmlel{children=[El]}) ->
    case exmpp_xml:get_attribute(El, <<"type">>, <<"entity">>) of
	<<"caps">> -> occi_query;
	<<"col">> -> occi_collection;
	<<"entity">> -> occi_entity;
	_ -> throw({error, invalid_type})
    end;
get_type(_El) ->
    lager:error("Invalid OCCI IQ: ~p~n", [lager:pr(_El, ?MODULE)]),
    throw({error, invalid_occi_iq}).

-spec get_op(xmlel()) -> get | save | update | delete.
get_op(#xmlel{children=[El]}=Iq) ->
    case exmpp_iq:get_type(Iq) of
	'get' -> get;
	'set' ->
	    case exmpp_xml:get_attribute(El, <<"action">>, <<"save">>) of
		<<"save">> -> save;
		<<"update">> -> update;
		<<"delete">> -> delete;
		_ -> throw({error, invalid_op})
	    end
    end;
get_op(_El) ->
    lager:error("Invalid OCCI IQ: ~p~n", [lager:pr(_El, ?MODULE)]),
    throw({error, invalid_occi_iq}).

-spec get_payload(occi_iq() | xmlel()) -> undefined | xmlel().
get_payload(#occi_iq{raw=Raw}) ->
    get_payload(Raw);
get_payload(#xmlel{}=Iq) ->
    case exmpp_xml:get_child_elements(Iq) of
	[] ->
	    undefined;
	[Result | _] ->
	    Result
    end.

-spec set_node_attr(occi_iq() | xmlel(), uri()) -> occi_iq() | xmlel().
set_node_attr(#xmlel{children=[El]}=Iq, #uri{path=Path}) ->
    El2 = exmpp_xml:set_attribute(El, <<"node">>, Path),
    Iq#xmlel{children=[El2]};
set_node_attr(#occi_iq{raw=Raw}=Iq, #uri{}=Uri) ->
    El = set_node_attr(Raw, Uri),
    Iq#occi_iq{raw=El}.

%%%
%%% Priv
%%%
iq_id() ->
    "iq-" ++ integer_to_list(random:uniform(65536 * 65536)).
