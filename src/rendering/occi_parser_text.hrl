-include("occi.hrl").

-type(req_type() :: action | entity | user_mixin | collection | filters).
-record(state, {type                                :: req_type(),
		collection                          :: occi_collection(),
		entity         = undefined          :: term(),
		entity_id      = undefined          :: uri(),
		mixin          = undefined          :: term(),
		action         = undefined          :: occi_action(),
		filters        = []                 :: occi_filters()}).
