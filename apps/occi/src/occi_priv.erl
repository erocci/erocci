%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2013 Jean Parpaillon.
%% @doc Agent setup API

-module(occi_priv).
-export([init/3, 
				 allowed_methods/2,
				 content_types_provided/2,
				 process_post/2]).
-export([get_json/2]).

-include("jsonerl.hrl").

-record(status, {http_port       = 0         :: pos_integer(),
								 xmpp_port       = 0         :: pos_integer(),
								 xmpp_jid        = <<>>      :: binary(),
								 xmpp_passwd     = <<>>      :: binary(),
								 xmpp_status     = undefined :: atom()}).

init(_Transport, _Req, []) ->
		{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, Ctx) ->
		{[<<"HEAD">>, <<"GET">>, <<"POST">>], Req, Ctx}.

content_types_provided(Req, Ctx) ->
		{[
			{<<"application/json">>, get_json}
		 ],
		 Req, Ctx}.

get_json(Req, Ctx) ->
		Status = get_status(),
		{?record_to_json(status, Status), Req, Ctx}.

process_post(Req, Ctx) ->
		{true, Req, Ctx}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_status() ->
		{ XMPP_PORT, XMPP_JID, XMPP_STATUS } = occi_xmpp:get_status(),
		#status{ http_port=occi_http:get_port(),
						 xmpp_port=XMPP_PORT,
						 xmpp_jid=XMPP_JID,
						 xmpp_status=XMPP_STATUS }.
