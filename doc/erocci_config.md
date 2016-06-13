

# Module erocci_config #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) (c) 2014-2016 Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###


<pre><code>
key() = listeners | backends | acl
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = [<a href="erocci_listener.md#type-t">erocci_listener:t()</a>] | [<a href="erocci_backend.md#type-t">erocci_backend:t()</a>] | [<a href="erocci_acl.md#type-t">erocci_acl:t()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Get a configuration value, eventually pre-processed
* <code>listeners -> [occi_listener:t()]</code>
* <code>backends -> [occi_backend:t()]</code>
* <code>acl -> [erocci_acl:t()]</code></td></tr><tr><td valign="top"><a href="#get_raw-2">get_raw/2</a></td><td>Get raw value from @see application:get_env.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(Key::<a href="#type-key">key()</a>) -&gt; <a href="#type-value">value()</a>
</code></pre>
<br />

Get a configuration value, eventually pre-processed
* `listeners -> [occi_listener:t()]`
* `backends -> [occi_backend:t()]`
* `acl -> [erocci_acl:t()]`

<a name="get_raw-2"></a>

### get_raw/2 ###

<pre><code>
get_raw(Key::<a href="#type-key">key()</a>, Default::term()) -&gt; term()
</code></pre>
<br />

Get raw value from @see application:get_env

