%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Created from https://github.com/dizz/occi-grammar/blob/master/occi-antlr-grammar/Occi.g
%%%
%%% @end
%%% Created : 14 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>

-define(OCCI_BASE_SCHEME, <<"http://schemas.ogf.org/occi/">>).

-type uri() :: binary().

%% OCCI Attributes
-record(occi_attr, {key   = undefined :: atom(),
										value = undefined :: any(),
										prop  = undefined :: undefined | immutable | required}).
-type occi_attr() :: #occi_attr{}.

%%% OCCI Category
-record(occi_category, {scheme    = <<>>      :: uri(),
												term      = undefined :: atom(),
												title     = <<>>      :: binary(),
												location  = <<>>      :: uri(),
												attrs     = []        :: [ occi_attr() ]}).
-type occi_category() :: #occi_category{}.

%%% OCCI Action
-record(occi_action, {super = #occi_category{} :: occi_category()}).
-type occi_action() :: #occi_action{}.

%%% OCCI Kind
-record(occi_kind, {super   = #occi_category{} :: occi_category(),
										actions = []               :: [ occi_action() ],
										rel     = <<>>             :: uri()}).
-type occi_kind() :: #occi_kind{}.

%%% OCCI Mixin
-record(occi_mixin, {super   = #occi_category{} :: occi_category(),
										 actions = []               :: [ occi_action() ],
										 rel     = <<>>             :: uri()}).
-type occi_mixin() :: #occi_mixin{}.

%%% OCCI Entity
-record(occi_entity, {id     = <<>>         :: uri(),
											title  = <<>>         :: binary(),
											kind   = #occi_kind{} :: occi_kind(),
											mixins = []           :: [ occi_mixin() ]}).
-type occi_entity() :: #occi_entity{}.

%%% OCCI Link
-record(occi_link, {super  = #occi_entity{} :: occi_entity(),
										target = <<>>           :: uri(),
										source = <<>>           :: uri()}).
-type occi_link() :: #occi_link{}.

%%% OCCI Resource
-record(occi_resource, {super   = #occi_entity{} :: occi_entity(),
												summary = <<>>           :: binary(),
												links   = []             :: [ uri() ]}).
-type occi_resource() :: #occi_resource{}.
