

# Module occi_entity #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2014, Jean Parpaillon

This file is provided to you under the Apache License,
Version 2.0 (the "License"); you may not use this file
except in compliance with the License.  You may obtain
a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-t">t()</a> ###


<pre><code>
t() = <a href="#type-occi_resource">occi_resource()</a> | <a href="#type-occi_link">occi_link()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_mixin-2">add_mixin/2</a></td><td></td></tr><tr><td valign="top"><a href="#del_mixin-2">del_mixin/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_mixins-1">get_mixins/1</a></td><td></td></tr><tr><td valign="top"><a href="#has_category-2">has_category/2</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td></td></tr><tr><td valign="top"><a href="#id-2">id/2</a></td><td></td></tr><tr><td valign="top"><a href="#match_attr-3">match_attr/3</a></td><td></td></tr><tr><td valign="top"><a href="#merge_attrs-2">merge_attrs/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td></td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td></td></tr><tr><td valign="top"><a href="#rm_attrs-2">rm_attrs/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_attr_value-3">set_attr_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_id-2">set_id/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_mixin-2"></a>

### add_mixin/2 ###

<pre><code>
add_mixin(Occi_resource::<a href="#type-occi_resource">occi_resource()</a> | <a href="#type-occi_link">occi_link()</a>, Mixin::<a href="#type-occi_mixin">occi_mixin()</a>) -&gt; <a href="#type-occi_resource">occi_resource()</a> | <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="del_mixin-2"></a>

### del_mixin/2 ###

<pre><code>
del_mixin(Occi_resource::<a href="#type-occi_resource">occi_resource()</a> | <a href="#type-occi_link">occi_link()</a>, Mixin::<a href="#type-occi_mixin">occi_mixin()</a>) -&gt; <a href="#type-occi_resource">occi_resource()</a> | <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="get_mixins-1"></a>

### get_mixins/1 ###

<pre><code>
get_mixins(Occi_resource::<a href="#type-occi_resource">occi_resource()</a> | <a href="#type-occi_link">occi_link()</a>) -&gt; term()
</code></pre>
<br />

<a name="has_category-2"></a>

### has_category/2 ###

<pre><code>
has_category(Occi_resource::<a href="#type-occi_resource">occi_resource()</a> | <a href="#type-occi_link">occi_link()</a>, Cid::<a href="#type-occi_cid">occi_cid()</a>) -&gt; true | false
</code></pre>
<br />

<a name="id-1"></a>

### id/1 ###

<pre><code>
id(Occi_resource::<a href="#type-occi_resource">occi_resource()</a> | <a href="#type-occi_link">occi_link()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="id-2"></a>

### id/2 ###

`id(Res, Id) -> any()`

<a name="match_attr-3"></a>

### match_attr/3 ###

<pre><code>
match_attr(Occi_resource::<a href="#type-occi_resource">occi_resource()</a> | <a href="#type-occi_link">occi_link()</a>, Name::binary() | atom(), Val::binary()) -&gt; true | false
</code></pre>
<br />

<a name="merge_attrs-2"></a>

### merge_attrs/2 ###

`merge_attrs(Occi_kind, Attrs) -> any()`

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Occi_kind::<a href="#type-occi_kind">occi_kind()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::<a href="#type-uri">uri()</a>, Occi_kind::<a href="#type-occi_kind">occi_kind()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="new-4"></a>

### new/4 ###

<pre><code>
new(Id::<a href="occi_uri.md#type-t">occi_uri:t()</a>, KindId::<a href="occi_cid.md#type-t">occi_cid:t()</a> | binary(), MixinIds::[<a href="occi_cid.md#type-t">occi_cid:t()</a> | binary()], Attrs::[{<a href="occi_attribute.md#type-key">occi_attribute:key()</a>, <a href="occi_attribute.md#type-value">occi_attribute:value()</a>}]) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="reset-1"></a>

### reset/1 ###

<pre><code>
reset(Occi_resource::<a href="#type-occi_entity">occi_entity()</a>) -&gt; <a href="#type-occi_entity">occi_entity()</a>
</code></pre>
<br />

<a name="rm_attrs-2"></a>

### rm_attrs/2 ###

`rm_attrs(Occi_mixin, Attrs) -> any()`

<a name="set_attr_value-3"></a>

### set_attr_value/3 ###

<pre><code>
set_attr_value(Occi_resource::<a href="#type-occi_resource">occi_resource()</a> | <a href="#type-occi_link">occi_link()</a>, Name::atom(), Value::term()) -&gt; <a href="#type-occi_resource">occi_resource()</a> | <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="set_id-2"></a>

### set_id/2 ###

<pre><code>
set_id(Occi_resource::<a href="#type-occi_resource">occi_resource()</a> | <a href="#type-occi_link">occi_link()</a>, Id::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

