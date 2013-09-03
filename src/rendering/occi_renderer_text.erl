%%%-------------------------------------------------------------------
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
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_renderer_text).
-compile({parse_transform, lager_transform}).

-include("occi.hrl").

%% API
-export([render/2]).

%%%===================================================================
%%% API
%%%===================================================================
render(#occi_kind{}=Kind, Sep) ->
    occi_renderer:join(
      occi_renderer:join([render_cid(Kind#occi_kind.id, Sep),
			  render_kv(<<"title">>, [Kind#occi_kind.title]),
			  render_kv(<<"rel">>, render_rel(Kind#occi_kind.rel)),
			  render_kv(<<"attributes">>, lists:map(fun(X) -> render_attr_spec(X) end, Kind#occi_kind.attributes)),
			  render_kv(<<"actions">>, lists:map(fun(X) -> render_action_spec(X) end, Kind#occi_kind.actions)),
			  render_kv(<<"location">>, [Kind#occi_kind.location])], 
			 <<"; ">>), 
      Sep);
render(#occi_mixin{}=Mixin, Sep) ->
    occi_renderer:join(
      occi_renderer:join([render_cid(Mixin#occi_mixin.id, Sep),
			  render_kv(<<"title">>, [Mixin#occi_mixin.title]),
			  render_kv(<<"attributes">>, lists:map(fun(X) -> render_attr_spec(X) end, Mixin#occi_mixin.attributes)),
			  render_kv(<<"actions">>, lists:map(fun(X) -> render_action_spec(X) end, Mixin#occi_mixin.actions)),
			  render_kv(<<"location">>, [Mixin#occi_mixin.location])], 
			 <<"; ">>), 
      Sep);
render(#occi_action{}=Action, Sep) ->
    occi_renderer:join(
      occi_renderer:join([render_cid(Action#occi_action.id, Sep),
			  render_kv(<<"title">>, [Action#occi_action.title]),
			  render_kv(<<"attributes">>, lists:map(fun(X) -> render_attr_spec(X) end, Action#occi_action.attributes))],
			 <<"; ">>),
      Sep);
render(O, _Sep) ->
    lager:error("Invalid value: ~p~n", [O]),
    throw({error, {occi_syntax, "invalid value"}}).

render_cid(#occi_cid{scheme=Scheme}=Cid, Sep) when is_atom(Scheme) ->
    render_cid(Cid#occi_cid{scheme=atom_to_list(Scheme)}, Sep);
render_cid(#occi_cid{}=Cid, Sep) ->
    occi_renderer:join(
      occi_renderer:join([[atom_to_list(Cid#occi_cid.term)],
			  render_kv(<<"scheme">>, [Cid#occi_cid.scheme]),
			  render_kv(<<"class">>, [atom_to_list(Cid#occi_cid.class)]) 
			 ], 
			 <<"; ">>), 
      Sep).

render_attr_spec({K, [], _F}) ->
    atom_to_list(K);
render_attr_spec({K, L, _F}) ->
    [ atom_to_list(K), <<"{">>, occi_renderer:join(occi_renderer:to_list(L), ","), <<"}">> ];
render_attr_spec({K, _F}) ->
    atom_to_list(K).

render_action_spec({Scheme, Term, _Desc, _Attrs}) ->
    [ Scheme, atom_to_list(Term) ].

render_rel({Scheme, Term}) when is_atom(Scheme) ->
    render_rel({atom_to_list(Scheme), Term});
render_rel({Scheme, Term}) ->
    [[ Scheme, atom_to_list(Term) ]].

render_kv(_Key, undefined) ->
    [];
render_kv(_Key, <<>>) ->
    [];
render_kv(_Key, []) ->
    [];
render_kv(Key, Values) ->
    [Key, "=\"", occi_renderer:join(occi_renderer:to_list(Values), " "), "\""].
