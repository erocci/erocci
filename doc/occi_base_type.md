

# Module occi_base_type #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-spec">spec()</a> ###


__abstract datatype__: `spec()`




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cast-2">cast/2</a></td><td>Return value casted as the specified OCCI base type.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cast-2"></a>

### cast/2 ###

<pre><code>
cast(V::term(), X2::<a href="#type-spec">spec()</a>) -&gt; <a href="#type-t">t()</a> | {error, term()}
</code></pre>
<br />

throws `{invalid_value, [spec()](#type-spec), any()}`

Return value casted as the specified OCCI base type.
Throw error if value can not be casted. Do only syntactic checking.

