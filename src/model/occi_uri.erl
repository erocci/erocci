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
%%% Created : 20 Jan 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_uri).
-compile([{parse_transform, lager_transform}]).

-include("occi.hrl").

-export([parse/1,
	 to_iolist/1,
	 to_binary/1,
	 to_string/1]).

%%%
%%% API
%%%
parse(Uri) when is_binary(Uri) ->
    parse(binary_to_list(Uri));
parse([$/|Uri]) ->
    #uri{scheme=undefined, path=[$/|Uri]};
parse(Uri) ->
    case uri:parse(Uri) of
	{ok, {Scheme, UserInfo, Host, Port, Path, Query}} ->
	    #uri{scheme=Scheme, userinfo=UserInfo, host=Host, port=Port, path=Path, query=Query};
        {error, Err} ->
            throw({error, Err})
    end.

to_iolist(undefined) ->
    [];
to_iolist(#uri{scheme=undefined}=Uri) ->
    to_iolist(occi_config:to_url(Uri));
to_iolist(#uri{scheme=Scheme, userinfo=Auth, host=Host, port=Port, path=Path, query=Query}) ->
    uri:to_iolist({Scheme, Auth, Host, Port, Path, Query}).

to_binary(undefined) ->
    <<"">>;
to_binary(#uri{scheme=undefined}=Uri) ->
    to_binary(occi_config:to_url(Uri));
to_binary(#uri{scheme=Scheme, userinfo=Auth, host=Host, port=Port, path=Path, query=Query}) ->
    uri:to_binary({Scheme, Auth, Host, Port, Path, Query}).

to_string(undefined) ->
    [];
to_string(#uri{scheme=undefined}=Uri) ->
    to_string(occi_config:to_url(Uri));
to_string(#uri{scheme=Scheme, userinfo=Auth, host=Host, port=Port, path=Path, query=Query}) ->
    uri:to_string({Scheme, Auth, Host, Port, Path, Query}).
