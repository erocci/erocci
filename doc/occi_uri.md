

# Module occi_uri #
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
t() = <a href="#type-uri">uri()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_prefix-2">add_prefix/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#gen_id-2">gen_id/2</a></td><td></td></tr><tr><td valign="top"><a href="#gen_urn-2">gen_urn/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_parent-1">get_parent/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_rel-1">is_rel/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_root-1">is_root/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#rm_prefix-2">rm_prefix/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_binary-2">to_binary/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_iolist-1">to_iolist/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_iolist-2">to_iolist/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_string-1">to_string/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_string-2">to_string/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_url-2">to_url/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_prefix-2"></a>

### add_prefix/2 ###

<pre><code>
add_prefix(Uri::<a href="#type-uri">uri()</a>, Prefix::string()) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="decode-1"></a>

### decode/1 ###

`decode(Bin) -> any()`

<a name="encode-1"></a>

### encode/1 ###

`encode(Bin) -> any()`

<a name="gen_id-2"></a>

### gen_id/2 ###

<pre><code>
gen_id(Prefix::string() | binary(), Name::string()) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="gen_urn-2"></a>

### gen_urn/2 ###

<pre><code>
gen_urn(Nid::string(), Seed::string()) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="get_parent-1"></a>

### get_parent/1 ###

`get_parent(Uri) -> any()`

<a name="is_rel-1"></a>

### is_rel/1 ###

<pre><code>
is_rel(Uri::<a href="#type-uri">uri()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="is_root-1"></a>

### is_root/1 ###

`is_root(Uri) -> any()`

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Host::binary(), Path::binary(), QsVals::[{binary(), term()}]) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Uri::undefined | binary()) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="rm_prefix-2"></a>

### rm_prefix/2 ###

<pre><code>
rm_prefix(Uri::<a href="#type-uri">uri()</a>, Prefix::string()) -&gt; <a href="#type-uri">uri()</a>
</code></pre>
<br />

<a name="to_binary-1"></a>

### to_binary/1 ###

`to_binary(Uri) -> any()`

<a name="to_binary-2"></a>

### to_binary/2 ###

`to_binary(Uri, Occi_env) -> any()`

<a name="to_iolist-1"></a>

### to_iolist/1 ###

`to_iolist(Uri) -> any()`

<a name="to_iolist-2"></a>

### to_iolist/2 ###

`to_iolist(Uri, Occi_env) -> any()`

<a name="to_string-1"></a>

### to_string/1 ###

`to_string(Uri) -> any()`

<a name="to_string-2"></a>

### to_string/2 ###

`to_string(Uri, Occi_env) -> any()`

<a name="to_url-2"></a>

### to_url/2 ###

<pre><code>
to_url(Endpoint::#uri{}, Uri::#uri{}) -&gt; #uri{}
</code></pre>
<br />

