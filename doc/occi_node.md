

# Module occi_node #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_prefix-2">add_prefix/2</a></td><td></td></tr><tr><td valign="top"><a href="#data-1">data/1</a></td><td></td></tr><tr><td valign="top"><a href="#data-2">data/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_data-1">get_data/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_objid-1">get_objid/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_parent-1">get_parent/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_type-1">get_type/1</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#objid-1">objid/1</a></td><td></td></tr><tr><td valign="top"><a href="#owner-1">owner/1</a></td><td></td></tr><tr><td valign="top"><a href="#rm_prefix-2">rm_prefix/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_data-2">set_data/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_type-2">set_type/2</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_prefix-2"></a>

### add_prefix/2 ###

<pre><code>
add_prefix(Occi_node::<a href="#type-occi_node">occi_node()</a>, Prefix::list()) -&gt; <a href="#type-occi_node">occi_node()</a>
</code></pre>
<br />

<a name="data-1"></a>

### data/1 ###

<pre><code>
data(Occi_node::<a href="#type-occi_node">occi_node()</a>) -&gt; term()
</code></pre>
<br />

<a name="data-2"></a>

### data/2 ###

`data(Node, Data) -> any()`

<a name="get_data-1"></a>

### get_data/1 ###

<pre><code>
get_data(Occi_node::<a href="#type-occi_node">occi_node()</a>) -&gt; term()
</code></pre>
<br />

<a name="get_objid-1"></a>

### get_objid/1 ###

<pre><code>
get_objid(Occi_node::<a href="#type-occi_node">occi_node()</a>) -&gt; any()
</code></pre>
<br />

<a name="get_parent-1"></a>

### get_parent/1 ###

<pre><code>
get_parent(Occi_node::<a href="#type-occi_node">occi_node()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="get_type-1"></a>

### get_type/1 ###

<pre><code>
get_type(Occi_node::<a href="#type-occi_node">occi_node()</a>) -&gt; <a href="#type-occi_node_type">occi_node_type()</a>
</code></pre>
<br />

<a name="id-1"></a>

### id/1 ###

<pre><code>
id(Occi_node::<a href="#type-occi_node">occi_node()</a>) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Uri::<a href="#type-occi_node_id">occi_node_id()</a>, Occi_backend::<a href="#type-occi_node_type">occi_node_type()</a> | <a href="#type-occi_object">occi_object()</a>) -&gt; <a href="#type-occi_node">occi_node()</a>
</code></pre>
<br />

<a name="objid-1"></a>

### objid/1 ###

`objid(Occi_node) -> any()`

<a name="owner-1"></a>

### owner/1 ###

<pre><code>
owner(Occi_node::<a href="#type-occi_node">occi_node()</a>) -&gt; term()
</code></pre>
<br />

<a name="rm_prefix-2"></a>

### rm_prefix/2 ###

<pre><code>
rm_prefix(Occi_node::<a href="#type-occi_node">occi_node()</a>, Prefix::list()) -&gt; <a href="#type-occi_node">occi_node()</a>
</code></pre>
<br />

<a name="set_data-2"></a>

### set_data/2 ###

<pre><code>
set_data(Occi_node::<a href="#type-occi_node">occi_node()</a>, Occi_resource::term()) -&gt; <a href="#type-occi_node">occi_node()</a>
</code></pre>
<br />

<a name="set_type-2"></a>

### set_type/2 ###

<pre><code>
set_type(Occi_node::<a href="#type-occi_node">occi_node()</a>, Type::<a href="#type-occi_node_type">occi_node_type()</a>) -&gt; <a href="#type-occi_node">occi_node()</a>
</code></pre>
<br />

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(Occi_node::<a href="#type-occi_node">occi_node()</a>) -&gt; <a href="#type-occi_node_type">occi_node_type()</a>
</code></pre>
<br />

