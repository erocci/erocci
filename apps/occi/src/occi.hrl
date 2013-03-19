%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Created from https://github.com/dizz/occi-grammar/blob/master/occi-antlr-grammar/Occi.g
%%%
%%% @end
%%% Created : 14 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>

-define(OCCI_BASE_SCHEME, <<"http://schemas.ogf.org/occi/">>).

-record(occi_category, {scheme, term, title, location, attrs=[]}).

-record(occi_kind, {super = #occi_category{}, actions, rel}).
-record(occi_mixin, {super = #occi_category{}, actions, rel}).
-record(occi_action, {super = #occi_category{}}).

-record(occi_entity, {id, title, kind, mixins}).
-record(occi_link, {super = #occi_entity{}, target, source}).
-record(occi_resource, {super = #occi_entity{}, summary, links}).

-record(occi_attr, {key, value, prop}).
