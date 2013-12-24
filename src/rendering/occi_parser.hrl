-record(token, {name               :: atom(),
	        data = undefined   :: term(),
		pos  = undefined   :: term()}).
-type(token() :: #token{}).
