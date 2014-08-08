-ifndef(occi_dbus_hrl).
-define(occi_dbus_hrl, true).

-include_lib("dbus/include/dbus.hrl").

-define(TYPE_UNDEFINED, 0).
-define(TYPE_CAPABILITIES, 1).
-define(TYPE_RESOURCE, 2).
-define(TYPE_LINK, 3).
-define(TYPE_BOUNDED_COLL, 4).
-define(TYPE_UNBOUNDED_COLL, 5).
-define(TYPE_MIXIN, 6).

-define(dbus_undefined, #dbus_variant{type=boolean, value=false}).

-endif.
