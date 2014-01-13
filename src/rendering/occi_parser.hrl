-record(token, {name               :: atom(),
	        data = undefined   :: term(),
		pos  = undefined   :: term()}).
-type(token() :: #token{}).

-type(parser_id() :: reference()).
-type(parser_result() :: {ok, term()} | {error, term()}).

-record(parser, {id                      :: parser_id(),
		 mod                     :: atom(),
		 sink        = undefined :: parser(),
		 src         = undefined :: parser(),
		 stack       = []        :: [atom()],
		 state                   :: term()}).
-type(parser() :: #parser{}).

-define(get_state(Parser), Parser#parser.state).
-define(set_state(Parser, State), Parser#parser{state=State}).
