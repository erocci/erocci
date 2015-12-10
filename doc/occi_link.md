

# Module occi_link #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2013, Jean Parpaillon

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

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_mixin-2">add_mixin/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_prefix-2">add_prefix/2</a></td><td></td></tr><tr><td valign="top"><a href="#del_mixin-2">del_mixin/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_attr-2">get_attr/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_attr_value-2">get_attr_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_attributes-1">get_attributes/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_cid-1">get_cid/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_id-1">get_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_mixins-1">get_mixins/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_source-1">get_source/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_target-1">get_target/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_target_cid-1">get_target_cid/1</a></td><td></td></tr><tr><td valign="top"><a href="#has_category-2">has_category/2</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td></td></tr><tr><td valign="top"><a href="#match_attr-3">match_attr/3</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-6">new/6</a></td><td></td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td></td></tr><tr><td valign="top"><a href="#rm_prefix-2">rm_prefix/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_attr_value-3">set_attr_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_cid-2">set_cid/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_id-2">set_id/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_source-2">set_source/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_target-2">set_target/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_target_cid-2">set_target_cid/2</a></td><td></td></tr><tr><td valign="top"><a href="#target_cid-1">target_cid/1</a></td><td></td></tr><tr><td valign="top"><a href="#update_attr_value-2">update_attr_value/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_mixin-2"></a>

### add_mixin/2 ###

<pre><code>
add_mixin(Occi_link::<a href="#type-occi_link">occi_link()</a>, Mixin::<a href="#type-occi_mixin">occi_mixin()</a>) -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="add_prefix-2"></a>

### add_prefix/2 ###

<pre><code>
add_prefix(Occi_link::<a href="#type-occi_link">occi_link()</a>, Prefix::string()) -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="del_mixin-2"></a>

### del_mixin/2 ###

<pre><code>
del_mixin(Occi_link::<a href="#type-occi_link">occi_link()</a>, Occi_mixin::<a href="#type-occi_mixin">occi_mixin()</a>) -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="get_attr-2"></a>

### get_attr/2 ###

<pre><code>
get_attr(Occi_link::<a href="#type-occi_link">occi_link()</a>, Key::<a href="#type-occi_attr_key">occi_attr_key()</a>) -&gt; any()
</code></pre>
<br />

<a name="get_attr_value-2"></a>

### get_attr_value/2 ###

`get_attr_value(Occi_link, Key) -> any()`

<a name="get_attributes-1"></a>

### get_attributes/1 ###

<pre><code>
get_attributes(Occi_link::<a href="#type-occi_link">occi_link()</a>) -&gt; [<a href="#type-occi_attr">occi_attr()</a>]
</code></pre>
<br />

<a name="get_cid-1"></a>

### get_cid/1 ###

<pre><code>
get_cid(Occi_link::<a href="#type-occi_link">occi_link()</a>) -&gt; <a href="#type-occi_cid">occi_cid()</a>
</code></pre>
<br />

<a name="get_id-1"></a>

### get_id/1 ###

<pre><code>
get_id(Occi_link::<a href="#type-occi_link">occi_link()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="get_mixins-1"></a>

### get_mixins/1 ###

<pre><code>
get_mixins(Occi_link::<a href="#type-occi_link">occi_link()</a>) -&gt; term()
</code></pre>
<br />

<a name="get_source-1"></a>

### get_source/1 ###

<pre><code>
get_source(Occi_link::<a href="#type-occi_link">occi_link()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="get_target-1"></a>

### get_target/1 ###

<pre><code>
get_target(Occi_link::<a href="#type-occi_link">occi_link()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="get_target_cid-1"></a>

### get_target_cid/1 ###

<pre><code>
get_target_cid(Occi_link::<a href="#type-occi_link">occi_link()</a>) -&gt; <a href="#type-occi_cid">occi_cid()</a>
</code></pre>
<br />

<a name="has_category-2"></a>

### has_category/2 ###

<pre><code>
has_category(Occi_link::<a href="#type-occi_link">occi_link()</a>, Cid::<a href="#type-occi_cid">occi_cid()</a>) -&gt; true | false
</code></pre>
<br />

<a name="id-1"></a>

### id/1 ###

`id(Occi_link) -> any()`

<a name="match_attr-3"></a>

### match_attr/3 ###

<pre><code>
match_attr(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>, Name::binary() | atom(), Val::binary()) -&gt; true | false
</code></pre>
<br />

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Occi_kind::<a href="#type-occi_kind">occi_kind()</a> | <a href="#type-uri">uri()</a>) -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::<a href="#type-occi_objid">occi_objid()</a>, Kind::<a href="#type-occi_kind">occi_kind()</a>) -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="new-6"></a>

### new/6 ###

<pre><code>
new(Id::<a href="#type-occi_objid">occi_objid()</a>, Occi_kind::<a href="#type-occi_kind">occi_kind()</a>, Mixins::[<a href="#type-occi_mixin">occi_mixin()</a>], Attributes::[{atom(), term}], Source::<a href="#type-uri">uri()</a>, Target::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="reset-1"></a>

### reset/1 ###

<pre><code>
reset(Occi_link::<a href="#type-occi_link">occi_link()</a>) -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="rm_prefix-2"></a>

### rm_prefix/2 ###

<pre><code>
rm_prefix(Occi_link::<a href="#type-occi_link">occi_link()</a>, Prefix::string()) -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="set_attr_value-3"></a>

### set_attr_value/3 ###

<pre><code>
set_attr_value(Occi_link::<a href="#type-occi_link">occi_link()</a>, Key::<a href="#type-occi_attr_key">occi_attr_key()</a>, Val::any()) -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="set_cid-2"></a>

### set_cid/2 ###

<pre><code>
set_cid(Occi_link::<a href="#type-occi_link">occi_link()</a>, Occi_kind::<a href="#type-occi_kind">occi_kind()</a>) -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="set_id-2"></a>

### set_id/2 ###

<pre><code>
set_id(Occi_link::<a href="#type-occi_link">occi_link()</a>, Id::<a href="#type-occi_objid">occi_objid()</a>) -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="set_source-2"></a>

### set_source/2 ###

<pre><code>
set_source(Occi_link::<a href="#type-occi_link">occi_link()</a>, Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="set_target-2"></a>

### set_target/2 ###

<pre><code>
set_target(Occi_link::<a href="#type-occi_link">occi_link()</a>, Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-occi_link">occi_link()</a>
</code></pre>
<br />

<a name="set_target_cid-2"></a>

### set_target_cid/2 ###

<pre><code>
set_target_cid(Occi_link::<a href="#type-occi_link">occi_link()</a>, Occi_cid::<a href="#type-occi_cid">occi_cid()</a>) -&gt; <a href="#type-occi_cid">occi_cid()</a>
</code></pre>
<br />

<a name="target_cid-1"></a>

### target_cid/1 ###

`target_cid(Occi_link) -> any()`

<a name="update_attr_value-2"></a>

### update_attr_value/2 ###

<pre><code>
update_attr_value(Occi_link::<a href="#type-occi_resource">occi_resource()</a>, List::term()) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

