

# Module occi_action #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_attribute-2">add_attribute/2</a></td><td></td></tr><tr><td valign="top"><a href="#check-1">check/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_attr_list-1">get_attr_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_attributes-1">get_attributes/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_class-1">get_class/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_id-1">get_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_scheme-1">get_scheme/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_term-1">get_term/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_title-1">get_title/1</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_attr_value-3">set_attr_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_title-2">set_title/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_attribute-2"></a>

### add_attribute/2 ###

`add_attribute(Occi_action, A) -> any()`

<a name="check-1"></a>

### check/1 ###

<pre><code>
check(Occi_action::<a href="#type-occi_action">occi_action()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="get_attr_list-1"></a>

### get_attr_list/1 ###

`get_attr_list(Occi_action) -> any()`

<a name="get_attributes-1"></a>

### get_attributes/1 ###

`get_attributes(Occi_action) -> any()`

<a name="get_class-1"></a>

### get_class/1 ###

`get_class(X1) -> any()`

<a name="get_id-1"></a>

### get_id/1 ###

`get_id(Occi_action) -> any()`

<a name="get_scheme-1"></a>

### get_scheme/1 ###

`get_scheme(Occi_action) -> any()`

<a name="get_term-1"></a>

### get_term/1 ###

`get_term(Occi_action) -> any()`

<a name="get_title-1"></a>

### get_title/1 ###

`get_title(Occi_action) -> any()`

<a name="id-1"></a>

### id/1 ###

`id(Occi_action) -> any()`

<a name="new-1"></a>

### new/1 ###

`new(Occi_cid) -> any()`

<a name="new-2"></a>

### new/2 ###

`new(Scheme, Term) -> any()`

<a name="set_attr_value-3"></a>

### set_attr_value/3 ###

<pre><code>
set_attr_value(Occi_action::<a href="#type-occi_action">occi_action()</a>, Key::<a href="#type-occi_attr_key">occi_attr_key()</a>, Val::any()) -&gt; <a href="#type-occi_action">occi_action()</a>
</code></pre>
<br />

<a name="set_title-2"></a>

### set_title/2 ###

`set_title(Occi_action, Title) -> any()`

