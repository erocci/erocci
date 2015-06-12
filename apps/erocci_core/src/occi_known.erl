-module(occi_known).

-export([scheme_to_atom/1,
	 term_to_atom/1,
	 class_to_atom/1,
	 attr_to_atom/1,
	 hdr_to_atom/1]).

-spec scheme_to_atom(binary()) -> atom() | binary().
scheme_to_atom(<<"http://schemas.ogf.org/occi/core#">>)        -> 'http://schemas.ogf.org/occi/core#';
scheme_to_atom(X) when is_binary(X)                            -> X.

-spec term_to_atom(binary()) -> atom() | binary().
term_to_atom(<<"resource">>)                   -> 'resource';
term_to_atom(<<"link">>)                       -> 'link';
term_to_atom(X) when is_binary(X)              -> X.

-spec class_to_atom(binary()) -> atom() | binary().
class_to_atom(<<"kind">>)                      -> 'kind';
class_to_atom(<<"mixin">>)                     -> 'mixin';
class_to_atom(<<"action">>)                    -> 'action'.

-spec attr_to_atom(binary()) -> atom() | binary().
attr_to_atom(<<"occi.core.id">>)               -> 'occi.core.id';
attr_to_atom(<<"occi.core.title">>)            -> 'occi.core.title';
attr_to_atom(<<"occi.core.summary">>)          -> 'occi.core.summary';
attr_to_atom(<<"occi.core.source">>)           -> 'occi.core.source';
attr_to_atom(<<"occi.core.target">>)           -> 'occi.core.target';
attr_to_atom(X) when is_binary(X)              -> X.

-spec hdr_to_atom(binary()) -> atom() | binary().
hdr_to_atom(<<"category">>)                    -> 'category';
hdr_to_atom(<<"x-occi-location">>)             -> 'x-occi-location';
hdr_to_atom(<<"x-occi-attribute">>)            -> 'x-occi-attribute';
hdr_to_atom(<<"link">>)                        -> 'link';
hdr_to_atom(<<"location">>)                    -> 'location';
hdr_to_atom(X) when is_binary(X)               -> X.
