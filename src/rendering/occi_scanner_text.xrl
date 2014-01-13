% -*- mode: erlang -*-
Definitions.

Quote         = "|\'
White         = (\s|\t|\n)

Url           = (http://|https://)([a-zA-Z0-9@:%_\\+.~#\?&/=-]*)
Path          = (/[a-zA-Z0-9-_]*)+
Term          = [a-zA-Z][a-zA-Z0-9-_]*
AttrName      = [a-zA-Z][a-zA-Z0-9]*(\.[a-zA-Z][a-zA-Z0-9]*)*
String        = "[^"]*"

Number        = [0-9]+
Float         = [0-9]+\.[0-9]+

Rules.

[:;,/<>{}=]                : make_atom(list_to_atom(TokenChars), TokenLine).
{Quote}                    : make_atom(quote, TokenLine).
{White}+                   : skip_token.

{Term}                     : make_term_or_atom(TokenChars, TokenLine).
\?action=                  : make_atom('?action=', TokenLine).
{AttrName}                 : make_token(attribute_name_attr, TokenLine, TokenChars).
{String}                   : make_token(quoted_value, TokenLine, unquote(TokenChars)).
{Number}                   : make_token(integer, TokenLine, list_to_integer(TokenChars)).
{Float}                    : make_token(float, TokenLine, list_to_float(TokenChars)).
{Url}                      : make_token(url, TokenLine, TokenChars).
{Path}                     : make_token(path, TokenLine, TokenChars).

Erlang code.

-export([scan/1]).

-spec scan(In :: binary()) -> Tokens :: list().
scan(In) ->
    case ?MODULE:string(binary_to_list(In)) of
	{ok, Tokens, _EndLine} ->
	    Tokens;
	ErrorInfo ->
	    throw(ErrorInfo)
    end.

make_token(Name, Line, Chars) ->
    {token, {Name, Line, Chars}}.

make_atom(Chars, Line) when is_list(Chars) ->
    {token, {list_to_atom(Chars), Line}};
make_atom(Atom, Line) when is_atom(Atom) ->
    {token, {Atom, Line}}.

make_term_or_atom(Chars, Line) ->
    Lower = string:to_lower(Chars),
    case is_reserved(Lower) of
	true  -> {token, {list_to_atom(Lower), Line}};
	false -> {token, {term, Line, Chars}}
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

unquote(Chars) ->
    lists:sublist(Chars, 2, length(Chars)-2).
