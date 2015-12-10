

# Module occi_collection #
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




### <a name="type-t">t()</a> ###


<pre><code>
t() = #occi_collection{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_entities-2">add_entities/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_entity-2">add_entity/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_prefix-2">add_prefix/2</a></td><td></td></tr><tr><td valign="top"><a href="#del_entities-2">del_entities/2</a></td><td></td></tr><tr><td valign="top"><a href="#del_entity-2">del_entity/2</a></td><td></td></tr><tr><td valign="top"><a href="#entities-1">entities/1</a></td><td></td></tr><tr><td valign="top"><a href="#fold-2">fold/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_entities-1">get_entities/1</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_empty-1">is_empty/1</a></td><td></td></tr><tr><td valign="top"><a href="#merge-2">merge/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#rm_prefix-2">rm_prefix/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_entities-2"></a>

### add_entities/2 ###

`add_entities(Occi_collection, E2) -> any()`

<a name="add_entity-2"></a>

### add_entity/2 ###

`add_entity(Occi_collection, Uri) -> any()`

<a name="add_prefix-2"></a>

### add_prefix/2 ###

<pre><code>
add_prefix(Occi_collection::<a href="#type-occi_collection">occi_collection()</a>, Prefix::string()) -&gt; <a href="#type-occi_collection">occi_collection()</a>
</code></pre>
<br />

<a name="del_entities-2"></a>

### del_entities/2 ###

`del_entities(Occi_collection, Uris) -> any()`

<a name="del_entity-2"></a>

### del_entity/2 ###

`del_entity(Occi_collection, Uri) -> any()`

<a name="entities-1"></a>

### entities/1 ###

`entities(Occi_collection) -> any()`

<a name="fold-2"></a>

### fold/2 ###

`fold(Occi_collection, F) -> any()`

<a name="get_entities-1"></a>

### get_entities/1 ###

`get_entities(Occi_collection) -> any()`

<a name="id-1"></a>

### id/1 ###

`id(Occi_collection) -> any()`

<a name="is_empty-1"></a>

### is_empty/1 ###

`is_empty(Occi_collection) -> any()`

<a name="merge-2"></a>

### merge/2 ###

`merge(Occi_collection, X2) -> any()`

<a name="new-0"></a>

### new/0 ###

`new() -> any()`

<a name="new-1"></a>

### new/1 ###

`new(Uri) -> any()`

<a name="new-2"></a>

### new/2 ###

`new(Uri, Elements) -> any()`

<a name="rm_prefix-2"></a>

### rm_prefix/2 ###

<pre><code>
rm_prefix(Occi_collection::<a href="#type-occi_collection">occi_collection()</a>, Prefix::string()) -&gt; <a href="#type-occi_collection">occi_collection()</a>
</code></pre>
<br />

