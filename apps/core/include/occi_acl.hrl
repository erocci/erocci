%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Created from https://github.com/dizz/occi-grammar/blob/master/occi-antlr-grammar/Occi.g
%%%
%%% @end
%%% Created : 14 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-ifndef(occi_acl_hrl).
-define(occi_acl_hrl, true).

-type(acl() :: {acl_policy(), acl_op(), acl_node(), acl_user()}).

-type(acl_policy() :: allow | deny).
-type(acl_op() :: create | read | update | {action, binary() } | delete | '_').
-type(acl_node() :: capabilities | acl_url()).
-type(acl_url() :: binary()).
-type(acl_user() :: anonymous | authenticated | admin | owner | group | {group, binary() } | '_').

-endif.