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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([parse/1,
	 gen_urn/2,
	 add_prefix/2,
	 rm_prefix/2,
	 get_parent/1,
	 is_root/1,
	 is_rel/1,
	 to_iolist/1,
	 to_binary/1,
	 to_string/1]).

% Wrappers to http_uri for treating with binaries
-export([encode/1,
	 decode/1]).

%%%
%%% API
%%%
parse(undefined) ->
    throw({error, invalid_uri});
parse(Uri) when is_binary(Uri) ->
    parse(binary_to_list(Uri));
parse([$u,$r,$n,$:|Uri]) ->
    #uri{scheme=urn, path=Uri};
parse([$/|Uri]) ->
    #uri{scheme=undefined, path=[$/|Uri]};
parse(Uri) ->
    case uri:parse(Uri) of
	{ok, {Scheme, UserInfo, Host, Port, Path, Query}} ->
	    #uri{scheme=Scheme, userinfo=UserInfo, host=Host, port=Port, path=Path, query=Query};
        {error, Err} ->
            throw({error, {Err, Uri}})
    end.

-spec gen_urn(Nid :: string(), Seed :: string()) -> uri().
gen_urn(Nid, Seed) ->
    Uuid = uuid:to_string(uuid:uuid3(oid, Seed)),
    #uri{scheme=urn, path=Nid ++ ":" ++ Uuid}.

-spec is_rel(uri()) -> boolean().
is_rel(#uri{path=Path}) ->
    case filename:pathtype(Path) of
	relative -> true;	    
	_ -> false
    end.

is_root(#uri{path=[]}) ->
    true;
is_root(#uri{path="/"}) ->
    true;
is_root(_) ->
    false.

%%%
%%% If URI's path is relative, add prefix
%%% If URI's path is absolute, does nothing
%%%
-spec add_prefix(uri(), string()) -> uri().
add_prefix(undefined, _) ->
    undefined;

add_prefix(#uri{scheme=urn}=Uri, _) ->
    Uri;

add_prefix(#uri{path=Path}=Uri, Prefix) ->
    case filename:split(Path) of
	["/"|_P] ->
	    Uri;
	_ ->
	    Uri#uri{path=filename:join(Prefix, Path)}
    end.

%%%
%%% Remove prefix and make path relative
%%%
-spec rm_prefix(uri(), string()) -> uri().
rm_prefix(undefined, _) ->
    undefined;

rm_prefix(#uri{scheme=urn}=Uri, _) ->
    Uri;

rm_prefix(#uri{path=Path}=Uri, Prefix) -> 
    case substr(Prefix, Path) of
	{ok, Path2} -> #uri{path=Path2};
	none -> Uri
    end.

get_parent(#uri{path=[]}) ->
    % parent of an empty path
    none;
get_parent(#uri{path=Path}=Uri) ->
    case lists:reverse(filename:split(Path)) of
	[_|[]] ->
	    % root's parent
	    Uri#uri{path=[]};
	[_|Parent] ->
	    Uri#uri{path=filename:join(lists:reverse(Parent))}
    end.

to_iolist(undefined) ->
    [];
to_iolist(#uri{scheme=undefined}=Uri) ->
    to_iolist(occi_config:to_url(Uri));
to_iolist(#uri{scheme='xmpp+occi', userinfo=U, host=H, path=P, 'query'=Q}) ->
    ["xmpp+occi:", U, "@", H, P, Q];
to_iolist(#uri{scheme=urn, path=Path}) ->
    ["urn:", Path];
to_iolist(#uri{scheme=Scheme, userinfo=Auth, host=Host, port=Port, path=Path, query=Query}) ->
    uri:to_iolist({Scheme, Auth, Host, Port, Path, Query}).

to_binary(Uri) ->
    iolist_to_binary(to_iolist(Uri)).

to_string(Uri) ->
    binary_to_list(to_binary(Uri)).

encode(Bin) ->
    list_to_binary(http_uri:encode(binary_to_list(Bin))).

decode(Bin) ->
    list_to_binary(http_uri:decode(binary_to_list(Bin))).

%%%
%%% Private
%%%
substr(S1, S2) ->
    substr(filename:split(S1), filename:split(S2), false).

substr(["/"], ["/", C| S2], false) ->
    {ok, filename:join([C|S2])};
substr(["/", C| S1], ["/", C| S2], false) ->
    substr(S1, S2, true);
substr(["/", _C1| _S1], ["/", _C2| _S2], false) ->
    none;
substr([C|S1], [C|S2], _MakeRel) ->
    substr(S1, S2, true);
substr([], [], true) ->
    {ok, []};
substr([], S2, true) ->
    {ok, filename:join(S2)};
substr(_S1, _S2, false) ->
    none.

%%%
%%% EUNIT
%%%
-ifdef(TEST).

add_prefix_test_() ->
    AbsUri = #uri{path="/path/to/a/resource"},
    RelUri = #uri{path="path/to/a/resource"},
    [
     ?_assert(add_prefix(AbsUri, "/a/prefix") =:= AbsUri),
     ?_assert(add_prefix(RelUri, "/a/prefix") =:= #uri{path="/a/prefix/path/to/a/resource"}),
     ?_assert(add_prefix(RelUri, "/a/prefix/") =:= #uri{path="/a/prefix/path/to/a/resource"}),
     ?_assert(add_prefix(RelUri, "/a/prefix//") =:= #uri{path="/a/prefix/path/to/a/resource"})
    ].

rm_prefix_tesst_() ->
    Uri = occi_uri:parse("http://example.com:99/what/a/prefix/path/to/a/resource"),
    Uri2 = occi_uri:parse("/resource"),
    [
     ?_assert(rm_prefix(Uri, "/what/a/prefix") =:= #uri{path="path/to/a/resource"}),
     ?_assert(rm_prefix(Uri, "/what/a/prefix/") =:= #uri{path="path/to/a/resource"}),
     ?_assert(rm_prefix(Uri, "/what/a/prefix//") =:= #uri{path="path/to/a/resource"}),
     ?_assert(rm_prefix(Uri, "/other/prefix") =:= 
		  #uri{scheme=http, host="example.com", port=99, path="/what/a/prefix/path/to/a/resource"}),
     ?_assert(rm_prefix(Uri, "/what/a/prefix") =:= 
		  #uri{scheme=http, host="example.com", port=99, path="/what/a/prefix/path/to/a/resource"}),
     ?_assert(rm_prefix(Uri, "/") =:= #uri{path="what/a/prefix/path/to/a/resource"}),
     ?_assert(rm_prefix(Uri2, "/a/long/prefix") =:= #uri{path="/resource"}),
     ?_assert(rm_prefix(Uri2, "/resource/") =:= #uri{path="/resource"})
    ].
    
-endif.
