%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Created from https://github.com/dizz/occi-grammar/blob/master/occi-antlr-grammar/Occi.g
%%%
%%% @end
%%% Created : 14 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-import_class(occi_category).

-type(uri() :: binary()).
% {Module, Function}
-type(occi_cb() :: {atom(), atom()}).

%% OCCI Attributes
-type(occi_attr_key() :: atom()).
-type(occi_attr_property() :: immutable | required).
-record(occi_attr, {key                    :: occi_attr_key(),
		    properties = []        :: [ occi_attr_property() ],
		    cb         = undefined :: occi_cb() | undefined  % used for type checking
		   }).
-type(occi_attr() :: #occi_attr{}).

%%% OCCI Category ID
-record(occi_category_id, {scheme    = undefined :: atom(),
			   term      = undefined :: atom()}).
-type(occi_category_id() :: #occi_category_id{}).

%%% OCCI Type (Category or derivative)
-record(occi_type, {id         = #occi_category_id{} :: occi_category_id(),
		    module     = undefined           :: atom(),
		    backend    = undefined           :: atom()}).
-type(occi_type() :: #occi_type{}).

%%% OCCI Entity ID
-type(occi_entity_id() :: uri()).

%%% OCCI Link
-record(occi_link, {id     = <<>>                :: occi_entity_id(),
		    title  = <<>>                :: binary(),
		    kind   = #occi_category_id{} :: occi_category_id(),
		    mixins = []                  :: [ occi_category_id() ],
		    target = <<>>                :: uri(),
		    source = <<>>                :: uri()}).
-type(occi_link() :: #occi_link{}).

%%% OCCI Resource
-record(occi_resource, {id     = <<>>                :: occi_entity_id(),
			title  = <<>>                :: binary(),
			kind   = #occi_category_id{} :: occi_category_id(),
			mixins = []                  :: [ occi_category_id() ],
			summary = <<>>               :: binary(),
			links   = []                 :: [ uri() ]}).
-type(occi_resource() :: #occi_resource{}).

%%% OCCI Entity
-type(occi_entity() :: occi_resource() | occi_link()).

%%% OCCI Filter
-type(occi_filter() :: term()).
