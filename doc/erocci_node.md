

# Module erocci_node #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (c) 2013-2016 Jean Parpaillon

This file is provided to you under the license described
in the file LICENSE at the root of the project.

You can also download the LICENSE file from the following URL:
https://github.com/erocci/erocci/blob/master/LICENSE

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-serial">serial()</a> ###


<pre><code>
serial() = binary() | undefined
</code></pre>




### <a name="type-t">t()</a> ###


<pre><code>
t() = #{}
</code></pre>




### <a name="type-type">type()</a> ###


<pre><code>
type() = capabilities | {collection, kind} | {collection, mixin} | entity
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#capabilities-1">capabilities/1</a></td><td>Creates a capabilities node.</td></tr><tr><td valign="top"><a href="#data-1">data/1</a></td><td>Get node data.</td></tr><tr><td valign="top"><a href="#data-2">data/2</a></td><td>Set node data.</td></tr><tr><td valign="top"><a href="#entity-1">entity/1</a></td><td>Creates an entity node.</td></tr><tr><td valign="top"><a href="#entity-3">entity/3</a></td><td>Creates an entity node.</td></tr><tr><td valign="top"><a href="#entity-4">entity/4</a></td><td>Creates an entity node.</td></tr><tr><td valign="top"><a href="#group-1">group/1</a></td><td>Get node owner group.</td></tr><tr><td valign="top"><a href="#group-2">group/2</a></td><td>Set node owner group.</td></tr><tr><td valign="top"><a href="#location-1">location/1</a></td><td>Get node location.</td></tr><tr><td valign="top"><a href="#owner-1">owner/1</a></td><td>Get node owner.</td></tr><tr><td valign="top"><a href="#owner-2">owner/2</a></td><td>Set node owner.</td></tr><tr><td valign="top"><a href="#serial-1">serial/1</a></td><td>Get node serial (tag).</td></tr><tr><td valign="top"><a href="#serial-2">serial/2</a></td><td>Set node serial number (tag).</td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td>Get node type.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="capabilities-1"></a>

### capabilities/1 ###

<pre><code>
capabilities(Categories::[<a href="occi_category.md#type-t">occi_category:t()</a>]) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Creates a capabilities node

<a name="data-1"></a>

### data/1 ###

<pre><code>
data(X1::<a href="#type-t">t()</a>) -&gt; <a href="occi_type.md#type-t">occi_type:t()</a>
</code></pre>
<br />

Get node data

<a name="data-2"></a>

### data/2 ###

<pre><code>
data(Data::term(), Node::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Set node data

<a name="entity-1"></a>

### entity/1 ###

<pre><code>
entity(Path::<a href="occi_entity.md#type-t">occi_entity:t()</a> | binary()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Creates an entity node

<a name="entity-3"></a>

### entity/3 ###

<pre><code>
entity(Entity::<a href="occi_entity.md#type-t">occi_entity:t()</a>, Owner::<a href="erocci_creds.md#type-user">erocci_creds:user()</a>, Group::<a href="erocci_creds.md#type-group">erocci_creds:group()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Creates an entity node

<a name="entity-4"></a>

### entity/4 ###

<pre><code>
entity(Entity::<a href="occi_entity.md#type-t">occi_entity:t()</a>, Owner::<a href="erocci_creds.md#type-user">erocci_creds:user()</a>, Group::<a href="erocci_creds.md#type-group">erocci_creds:group()</a>, Serial::<a href="#type-serial">serial()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Creates an entity node

<a name="group-1"></a>

### group/1 ###

<pre><code>
group(N::<a href="#type-t">t()</a>) -&gt; <a href="erocci_creds.md#type-group">erocci_creds:group()</a>
</code></pre>
<br />

Get node owner group

<a name="group-2"></a>

### group/2 ###

<pre><code>
group(Group::<a href="erocci_creds.md#type-group">erocci_creds:group()</a>, N::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Set node owner group

<a name="location-1"></a>

### location/1 ###

<pre><code>
location(X1::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

Get node location

<a name="owner-1"></a>

### owner/1 ###

<pre><code>
owner(N::<a href="#type-t">t()</a>) -&gt; <a href="erocci_creds.md#type-user">erocci_creds:user()</a>
</code></pre>
<br />

Get node owner

<a name="owner-2"></a>

### owner/2 ###

<pre><code>
owner(Owner::<a href="erocci_creds.md#type-user">erocci_creds:user()</a>, N::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Set node owner

<a name="serial-1"></a>

### serial/1 ###

<pre><code>
serial(X1::<a href="#type-t">t()</a>) -&gt; <a href="#type-serial">serial()</a>
</code></pre>
<br />

Get node serial (tag)

<a name="serial-2"></a>

### serial/2 ###

<pre><code>
serial(Serial::<a href="#type-serial">serial()</a>, Node::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Set node serial number (tag)

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(X1::<a href="#type-t">t()</a>) -&gt; <a href="#type-type">type()</a>
</code></pre>
<br />

Get node type

