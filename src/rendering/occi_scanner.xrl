% -*- mode: erlang -*-
Definitions.

Quote         = "|\'
Whites        = (\s|\t)+

Url           = (http://|https://)([a-zA-Z0-9@:%_\\+.~#?&/=-]*)
Path          = (/[a-zA-Z0-9-_]*)+
Term          = [a-zA-Z0-9-_]+
AttrName      = [a-zA-Z0-9](\.[a-zA-Z0-9]+)*
String        = "[^"]*"

Number        = [0-9]+
Float         = [0-9](\.[0-9])+

Rules.

[:;,/<>{}=]              : {token, {list_to_atom(TokenChars), TokenLine}}.
{Quote}                    : {token, {quote, TokenLine}}.
{Whites}+                  : skip_token.

{Term}                     : make_token(TokenChars, TokenLine).
\?action=                  : {token, {'?action=', TokenLine}}.
{AttrName}                 : {token, {attribute_name_attr, TokenLine}}.
{String}                   : {token, {string, TokenChars, TokenLine}}.
{Number}                   : {token, {list_to_integer(TokenChars), TokenLine}}.
{Float}                    : {float, {list_to_float(TokenChars), TokenLine}}.
{Url}                      : {token, {url, TokenChars, TokenLine}}.
{Path}                     : {token, {path, TokenChars, TokenLine}}.

Erlang code.

-include_lib("eunit/include/eunit.hrl").

make_token(Chars, Line) ->
  Lower = string:to_lower(Chars),
  case is_reserved(Lower) of
    true  -> {token, {list_to_atom(Lower), Line}};
    false -> {token, {term, Chars, Line}}
  end.

is_reserved("category")         -> true;
is_reserved("link")             -> true;
is_reserved("attribute")        -> true;
is_reserved("location")         -> true;
is_reserved("scheme")           -> true;
is_reserved("class")            -> true;
is_reserved("title")            -> true;
is_reserved("rel")              -> true;
is_reserved("attributes")       -> true;
is_reserved("actions")          -> true;
is_reserved("self")             -> true;
is_reserved("x-occi-attribute") -> true;
is_reserved("x-occi-location")  -> true;
is_reserved(_)                  -> false.

%%%
%%% Tests
%%%
scan1_test() ->
    Input = "Category: compute; scheme=\"http://schemas.ogf.org/occi/infrastructure#\"; class=\"kind\"",
    Expect = {ok,[{category,1},
		  {':',1},
		  {term,"compute",1},
		  {';',1},
		  {scheme,1},
		  {'=',1},
		  {string,"\"http://schemas.ogf.org/occi/infrastructure#\"",1},
		  {';',1},
		  {class,1},
		  {'=',1},
		  {string,"\"kind\"",1}],
	      1},
    ?assert(occi_scanner:string(Input) =:= Expect).
