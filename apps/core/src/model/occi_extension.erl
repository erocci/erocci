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
%%% Created : 19 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_extension).

-include("occi.hrl").

-export([new/2,
	 get_location/1,
	 set_location/2,
	 get_name/1,
	 get_version/1,
	 get_categories/1,
	 get_kinds/1,
	 get_mixins/1,
	 get_actions/1,
	 add_kind/2,
	 add_mixin/2,
	 find/2]).

new(Name, Version) ->
    #occi_extension{name=Name, 
		    version=Version,
		    kinds=[],
		    mixins=[]}.

get_location(#occi_mixin{location=Uri}) ->
    Uri.

set_location(#occi_mixin{}=Mixin, Uri) ->
    Mixin#occi_mixin{location=Uri}.

get_name(#occi_extension{name=Name}) ->
    Name.

get_version(#occi_extension{version=Version}) ->
    Version.

get_categories(#occi_extension{kinds=Kinds, mixins=Mixins}) ->
    lists:flatten([Kinds, Mixins]).

get_kinds(#occi_extension{kinds=Kinds}) ->
    Kinds.

get_mixins(#occi_extension{mixins=Mixins}) ->
    Mixins.

get_actions(#occi_extension{}=Ext) ->
    lists:flatten([
		   lists:map(fun (Kind) -> occi_kind:get_actions(Kind) end,
			     get_kinds(Ext)),
		   lists:map(fun (Mixin) -> occi_mixin:get_actions(Mixin) end,
			     get_mixins(Ext))
		  ]).

add_kind(#occi_extension{kinds=Kinds}=Ext, Kind) ->
    Ext#occi_extension{kinds=[Kind|Kinds]}.

add_mixin(#occi_extension{mixins=Mixins}=Ext, Mixin) ->
    Ext#occi_extension{mixins=[Mixin|Mixins]}.

find(#occi_extension{kinds=Kinds, mixins=Mixins}, #occi_cid{class='_'}=Cid) ->
    find_category(lists:flatten([Kinds, Mixins]), Cid);
find(#occi_extension{kinds=Kinds}, #occi_cid{class=kind}=Cid) ->
    find_category(Kinds, Cid);
find(#occi_extension{mixins=Mixins}, #occi_cid{class=mixins}=Cid) ->
    find_category(Mixins, Cid).

%%%
%%% Private
%%%
find_category([#occi_kind{id=#occi_cid{scheme=Scheme, term=Term}}=Kind | _], 
	      #occi_cid{scheme=Scheme, term=Term}) ->
    [Kind];
find_category([#occi_mixin{id=#occi_cid{scheme=Scheme, term=Term}}=Mixin | _], 
	      #occi_cid{scheme=Scheme, term=Term}) ->
    [Mixin];
find_category([ _ | Categories], #occi_cid{}=Cid) ->
    find_category(Categories, Cid);
find_category([], #occi_cid{}=_Cid) ->
    [].

