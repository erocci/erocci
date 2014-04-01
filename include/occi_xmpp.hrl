-define(ns_occi_xmpp, 'http://schemas.ogf.org/occi-xmpp').

-type(occi_iq_op() :: 'get' | save | update | delete).
-type(occi_iq_type() :: entity | caps | col).

-record(occi_iq, {
	  raw              :: term(),
	  op   = 'get'     :: occi_iq_op(),
	  node             :: term(),
	  type = entity    :: occi_iq_type()
	 }).
-type(occi_iq() :: #occi_iq{}).
