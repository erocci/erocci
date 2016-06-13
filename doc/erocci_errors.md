

# Module erocci_errors #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-t">t()</a> ###


<pre><code>
t() = <a href="erocci_store.md#type-error">erocci_store:error()</a> | <a href="erocci_backend.md#type-error">erocci_backend:error()</a> | <a href="occi_rendering.md#type-error">occi_rendering:error()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#render-2">render/2</a></td><td>Render errors.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="render-2"></a>

### render/2 ###

<pre><code>
render(Mimetype::<a href="occi_utils.md#type-mimetype">occi_utils:mimetype()</a>, Errors::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

Render errors

