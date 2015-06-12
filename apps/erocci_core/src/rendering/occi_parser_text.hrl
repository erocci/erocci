-include("occi.hrl").

-type(req_type() :: action | entity | user_mixin | collection).
-record(state, {type                                :: req_type(),
		collection                          :: occi_collection(),
		entity         = undefined          :: term(),
		entity_id      = undefined          :: uri(),
		mixin          = undefined          :: term(),
		action         = undefined          :: occi_action()}).

-define(hdr_to_atom(X), occi_known:hdr_to_atom(X)).
