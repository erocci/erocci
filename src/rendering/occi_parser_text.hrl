-include("occi.hrl").

-record(state, {collection                          :: occi_collection(),
		entity         = undefined          :: term(),
		entity_id      = undefined          :: uri(),
		mixin          = undefined          :: term(),
		action         = undefined          :: occi_action()}).
