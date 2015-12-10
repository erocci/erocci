

# Module occi_resource #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_link-2">add_link/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_mixin-2">add_mixin/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_prefix-2">add_prefix/2</a></td><td></td></tr><tr><td valign="top"><a href="#del_mixin-2">del_mixin/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_attr-2">get_attr/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_attr_value-2">get_attr_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_attributes-1">get_attributes/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_cid-1">get_cid/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_id-1">get_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_links-1">get_links/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_links_size-1">get_links_size/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_mixins-1">get_mixins/1</a></td><td></td></tr><tr><td valign="top"><a href="#has_category-2">has_category/2</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td></td></tr><tr><td valign="top"><a href="#links-2">links/2</a></td><td></td></tr><tr><td valign="top"><a href="#match_attr-3">match_attr/3</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td></td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td></td></tr><tr><td valign="top"><a href="#rm_prefix-2">rm_prefix/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_attr_value-3">set_attr_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_cid-2">set_cid/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_id-2">set_id/2</a></td><td></td></tr><tr><td valign="top"><a href="#update_attr_value-2">update_attr_value/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_link-2"></a>

### add_link/2 ###

<pre><code>
add_link(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>, Uri::<a href="#type-uri">uri()</a>) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

<a name="add_mixin-2"></a>

### add_mixin/2 ###

<pre><code>
add_mixin(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>, Mixin::<a href="#type-occi_mixin">occi_mixin()</a>) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

<a name="add_prefix-2"></a>

### add_prefix/2 ###

<pre><code>
add_prefix(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>, Prefix::string()) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

<a name="del_mixin-2"></a>

### del_mixin/2 ###

<pre><code>
del_mixin(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>, Occi_mixin::<a href="#type-occi_mixin">occi_mixin()</a>) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

<a name="get_attr-2"></a>

### get_attr/2 ###

<pre><code>
get_attr(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>, Key::<a href="#type-occi_attr_key">occi_attr_key()</a>) -&gt; any()
</code></pre>
<br />

<a name="get_attr_value-2"></a>

### get_attr_value/2 ###

`get_attr_value(Occi_resource, Key) -> any()`

<a name="get_attributes-1"></a>

### get_attributes/1 ###

<pre><code>
get_attributes(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>) -&gt; [<a href="#type-occi_attr">occi_attr()</a>]
</code></pre>
<br />

<a name="get_cid-1"></a>

### get_cid/1 ###

<pre><code>
get_cid(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>) -&gt; <a href="#type-occi_cid">occi_cid()</a>
</code></pre>
<br />

<a name="get_id-1"></a>

### get_id/1 ###

<pre><code>
get_id(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="get_links-1"></a>

### get_links/1 ###

<pre><code>
get_links(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>) -&gt; [<a href="#type-uri">uri()</a>]
</code></pre>
<br />

<a name="get_links_size-1"></a>

### get_links_size/1 ###

<pre><code>
get_links_size(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>) -&gt; integer()
</code></pre>
<br />

<a name="get_mixins-1"></a>

### get_mixins/1 ###

<pre><code>
get_mixins(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>) -&gt; term()
</code></pre>
<br />

<a name="has_category-2"></a>

### has_category/2 ###

<pre><code>
has_category(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>, Cid::<a href="#type-occi_cid">occi_cid()</a>) -&gt; true | false
</code></pre>
<br />

<a name="id-1"></a>

### id/1 ###

`id(Occi_resource) -> any()`

<a name="links-2"></a>

### links/2 ###

`links(Occi_resource, Links) -> any()`

<a name="match_attr-3"></a>

### match_attr/3 ###

<pre><code>
match_attr(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>, Name::binary() | atom(), Val::binary()) -&gt; true | false
</code></pre>
<br />

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Occi_kind::<a href="#type-occi_kind">occi_kind()</a> | term()) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::<a href="#type-occi_objid">occi_objid()</a>, Kind::<a href="#type-occi_kind">occi_kind()</a>) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

<a name="new-4"></a>

### new/4 ###

<pre><code>
new(Id::<a href="#type-occi_objid">occi_objid()</a>, Kind::<a href="#type-occi_kind">occi_kind()</a>, Mixins::[<a href="#type-occi_mixin">occi_mixin()</a>], Attributes::[{Key::atom(), Val::term}]) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

<a name="reset-1"></a>

### reset/1 ###

<pre><code>
reset(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

<a name="rm_prefix-2"></a>

### rm_prefix/2 ###

<pre><code>
rm_prefix(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>, Prefix::string()) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

<a name="set_attr_value-3"></a>

### set_attr_value/3 ###

<pre><code>
set_attr_value(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>, Key::<a href="#type-occi_attr_key">occi_attr_key()</a>, Val::any()) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

<a name="set_cid-2"></a>

### set_cid/2 ###

<pre><code>
set_cid(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>, Occi_kind::<a href="#type-occi_kind">occi_kind()</a>) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

<a name="set_id-2"></a>

### set_id/2 ###

<pre><code>
set_id(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>, Id::<a href="#type-occi_objid">occi_objid()</a> | binary()) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

<a name="update_attr_value-2"></a>

### update_attr_value/2 ###

<pre><code>
update_attr_value(Occi_resource::<a href="#type-occi_resource">occi_resource()</a>, List::term()) -&gt; <a href="#type-occi_resource">occi_resource()</a>
</code></pre>
<br />

