

# Module occi_type #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-mod">mod()</a> ###


<pre><code>
mod() = occi_extension | occi_collection | occi_category | occi_kind | occi_mixin | occi_action | occi_attribute | occi_entity | occi_resource | occi_link | occi_invoke
</code></pre>




### <a name="type-name">name()</a> ###


<pre><code>
name() = extension | categories | collection | category | kind | mixin | action | attribute | entity | resource | link | invoke
</code></pre>




### <a name="type-t">t()</a> ###


<pre><code>
t() = <a href="occi_extension.md#type-t">occi_extension:t()</a> | <a href="occi_collection.md#type-t">occi_collection:t()</a> | <a href="occi_category.md#type-t">occi_category:t()</a> | <a href="occi_kind.md#type-t">occi_kind:t()</a> | <a href="occi_mixin.md#type-t">occi_mixin:t()</a> | <a href="occi_action.md#type-t">occi_action:t()</a> | <a href="occi_attribute.md#type-t">occi_attribute:t()</a> | <a href="occi_entity.md#type-t">occi_entity:t()</a> | <a href="occi_resource.md#type-t">occi_resource:t()</a> | <a href="occi_link.md#type-t">occi_link:t()</a> | <a href="occi_invoke.md#type-t">occi_invoke:t()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#mod-1">mod/1</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="mod-1"></a>

### mod/1 ###

<pre><code>
mod(T::<a href="#type-t">t()</a>) -&gt; <a href="#type-mod">mod()</a>
</code></pre>
<br />

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(T::<a href="#type-t">t()</a>) -&gt; <a href="#type-name">name()</a>
</code></pre>
<br />

