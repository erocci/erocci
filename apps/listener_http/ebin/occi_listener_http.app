%%-*- mode: erlang -*-
{application, occi_listener_http,
 [
  {description, "erocci HTTP listener"},
  {modules, [occi_http_common,occi_http,occi_http_handler,occi_https]},
  {vsn, "0.5"}
 ]}.
