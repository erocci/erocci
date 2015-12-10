

# Module occi_config #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Config has 2 sources:
- application env
- load/1 arg
Application env override load/1 env.

Copyright (c) (C) 2014, Jean Parpaillon

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##

Config properties:
- backends: a list of backends, loaded with occi_store:register/1
- name: base URI of the server. Normally, listeners are responsible for discovering name
(e.g.: http://localhost:8080)
- listeners: a list of listeners, loaded with occi_listener:register/1.
- backend_timeout: timeout after which a backend is considered dead. Default to 5000 (ms)
- categories_map: a function for mapping category id (occi_cid()) to an URI. Default to
occi_category_mgr:hash/1
- categories_prefix: prefix for collections, for the occi_category_mgr:hash/1 function.
Default: /collections

All other properties are stored in the config manager and accessible with get/1 and get/2.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#gen_id-2">gen_id/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#load-1">load/1</a></td><td>Config is a proplist, which can be overriden by application env.</td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="gen_id-2"></a>

### gen_id/2 ###

<pre><code>
gen_id(Prefix::string() | binary(), Occi_env::<a href="#type-occi_env">occi_env()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="get-1"></a>

### get/1 ###

`get(Name) -> any()`

<a name="get-2"></a>

### get/2 ###

`get(Name, Default) -> any()`

<a name="load-1"></a>

### load/1 ###

<pre><code>
load(Config::list()) -&gt; ok | {error, term()}
</code></pre>
<br />

Config is a proplist, which can be overriden by application env

<a name="set-2"></a>

### set/2 ###

`set(Name, Value) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

