

# Module occi_attribute #
* [Description](#description)
* [Data Types](#types)
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

<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###


<pre><code>
key() = <a href="#type-occi_attr_key">occi_attr_key()</a>
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = any()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_prefix-2">add_prefix/2</a></td><td></td></tr><tr><td valign="top"><a href="#check-1">check/1</a></td><td></td></tr><tr><td valign="top"><a href="#core_id-0">core_id/0</a></td><td></td></tr><tr><td valign="top"><a href="#core_src-0">core_src/0</a></td><td></td></tr><tr><td valign="top"><a href="#core_summary-0">core_summary/0</a></td><td></td></tr><tr><td valign="top"><a href="#core_target-0">core_target/0</a></td><td></td></tr><tr><td valign="top"><a href="#core_title-0">core_title/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_default-1">get_default/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_id-1">get_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_title-1">get_title/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_type-1">get_type/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_type_id-1">get_type_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-1">get_value/1</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_immutable-1">is_immutable/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_required-1">is_required/1</a></td><td></td></tr><tr><td valign="top"><a href="#match_value-2">match_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td></td></tr><tr><td valign="top"><a href="#rm_prefix-2">rm_prefix/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_default-2">set_default/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_immutable-2">set_immutable/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_required-2">set_required/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_title-2">set_title/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_type-2">set_type/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_value-2">set_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_prefix-2"></a>

### add_prefix/2 ###

`add_prefix(Occi_attr, Prefix) -> any()`

<a name="check-1"></a>

### check/1 ###

`check(Occi_attr) -> any()`

<a name="core_id-0"></a>

### core_id/0 ###

`core_id() -> any()`

<a name="core_src-0"></a>

### core_src/0 ###

`core_src() -> any()`

<a name="core_summary-0"></a>

### core_summary/0 ###

`core_summary() -> any()`

<a name="core_target-0"></a>

### core_target/0 ###

`core_target() -> any()`

<a name="core_title-0"></a>

### core_title/0 ###

`core_title() -> any()`

<a name="get_default-1"></a>

### get_default/1 ###

`get_default(Occi_attr) -> any()`

<a name="get_id-1"></a>

### get_id/1 ###

`get_id(A) -> any()`

<a name="get_title-1"></a>

### get_title/1 ###

`get_title(Occi_attr) -> any()`

<a name="get_type-1"></a>

### get_type/1 ###

`get_type(Occi_attr) -> any()`

<a name="get_type_id-1"></a>

### get_type_id/1 ###

`get_type_id(Occi_attr) -> any()`

<a name="get_value-1"></a>

### get_value/1 ###

`get_value(Occi_attr) -> any()`

<a name="id-1"></a>

### id/1 ###

`id(A) -> any()`

<a name="is_immutable-1"></a>

### is_immutable/1 ###

`is_immutable(A) -> any()`

<a name="is_required-1"></a>

### is_required/1 ###

`is_required(A) -> any()`

<a name="match_value-2"></a>

### match_value/2 ###

<pre><code>
match_value(Occi_attr::<a href="#type-occi_attr">occi_attr()</a>, M::binary()) -&gt; true | false
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

`new(Id) -> any()`

<a name="reset-1"></a>

### reset/1 ###

<pre><code>
reset(Occi_attr::<a href="#type-occi_attr">occi_attr()</a>) -&gt; <a href="#type-occi_attr">occi_attr()</a>
</code></pre>
<br />

<a name="rm_prefix-2"></a>

### rm_prefix/2 ###

`rm_prefix(Occi_attr, Prefix) -> any()`

<a name="set_default-2"></a>

### set_default/2 ###

`set_default(Occi_attr, Value) -> any()`

<a name="set_immutable-2"></a>

### set_immutable/2 ###

`set_immutable(Occi_attr, Val) -> any()`

<a name="set_required-2"></a>

### set_required/2 ###

`set_required(A, Req) -> any()`

<a name="set_title-2"></a>

### set_title/2 ###

`set_title(A, Title) -> any()`

<a name="set_type-2"></a>

### set_type/2 ###

`set_type(A, Type) -> any()`

<a name="set_value-2"></a>

### set_value/2 ###

`set_value(Occi_attr, Value) -> any()`

<a name="value-1"></a>

### value/1 ###

`value(Occi_attr) -> any()`

