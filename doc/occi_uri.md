

# Module occi_uri #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-prefix_op">prefix_op()</a> ###


<pre><code>
prefix_op() = add | rm
</code></pre>




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`




### <a name="type-url">url()</a> ###


<pre><code>
url() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#canonical-1">canonical/1</a></td><td>Make uri canonical: add default port if necessary.</td></tr><tr><td valign="top"><a href="#change_prefix-3">change_prefix/3</a></td><td>Change prefix of url.</td></tr><tr><td valign="top"><a href="#from_string-1">from_string/1</a></td><td>Parse uri.</td></tr><tr><td valign="top"><a href="#to_string-1">to_string/1</a></td><td>Render uri as binary.</td></tr><tr><td valign="top"><a href="#to_string-2">to_string/2</a></td><td>Render uri as binary, with a different context.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="canonical-1"></a>

### canonical/1 ###

<pre><code>
canonical(Bin::<a href="#type-url">url()</a>) -&gt; <a href="#type-url">url()</a>
</code></pre>
<br />

Make uri canonical: add default port if necessary

<a name="change_prefix-3"></a>

### change_prefix/3 ###

<pre><code>
change_prefix(X1::<a href="#type-prefix_op">prefix_op()</a>, Prefix::binary(), Path::binary()) -&gt; binary()
</code></pre>
<br />

Change prefix of url

<a name="from_string-1"></a>

### from_string/1 ###

<pre><code>
from_string(S::binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Parse uri

<a name="to_string-1"></a>

### to_string/1 ###

<pre><code>
to_string(Uri::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

Render uri as binary

<a name="to_string-2"></a>

### to_string/2 ###

<pre><code>
to_string(Uri::<a href="uri.md#type-t">uri:t()</a>, Ctx::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

Render uri as binary, with a different context

