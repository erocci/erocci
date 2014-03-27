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
%%% Created : 26 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_iq).
-compile({parse_transform, lager_transform}).

-include("occi.hrl").
-include("occi_xml.hrl").
-include_lib("erim/include/exmpp.hrl").
-include_lib("erim/include/exmpp_jid.hrl").

-export([q/2,
	 result/2]).

-export([get_node/1]).

-spec q(To :: #jid{}, Node :: uri() | binary()) -> xmlel().
q(#jid{raw=To}, #uri{path=Path}) ->
    q(To, Path);

q(To, Path) when is_binary(Path) ->
    Query = exmpp_xml:element(?occi_ns, 'query', [exmpp_xml:attribute(<<"node">>, Path)], []),
    Iq = ?IQ_GET(To, iq_id()),
    exmpp_xml:append_child(Iq, Query).

-spec result(Iq :: #xmlel{}, Node :: occi_node()) -> #xmlel{}.
result(#xmlel{}=Iq, #occi_node{}=Node) ->
    Res = exmpp_xml:element(?occi_ns, 'query',
			    [exmpp_xml:attribute(<<"node">>, get_path(Node))],
			    [occi_renderer_xml:to_xmlel(Node)]),
    exmpp_xml:append_child(exmpp_iq:result(Iq), Res).

-spec get_node(xmlel()) -> occi_node().
get_node(#xmlel{children=[El]}) ->
    case exmpp_xml:get_attribute(El, <<"node">>, undefined) of
	undefined ->
	    {error, undefined_occi_node};
	<<"/-/">> ->
	    #occi_node{type=occi_query, _='_'};
	Path ->
	    try occi_uri:parse(Path) of
		#uri{}=Uri -> #occi_node{id=Uri, _='_'}
	    catch throw:Err -> {error, Err}
	    end
    end;
get_node(_El) ->
    lager:error("Invalid OCCI IQ: ~p~n", [lager:pr(_El, ?MODULE)]),
    {error, invalid_occi_iq}.

%%%
%%% Priv
%%%
get_path(#occi_node{type=occi_query}) ->
    <<"/-/">>;
get_path(#occi_node{id=#uri{path=Path}}) ->
    Path.

iq_id() ->
    "iq-" ++ integer_to_list(random:uniform(65536 * 65536)).
