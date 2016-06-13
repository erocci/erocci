

# Module erocci_filter #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Represent an OCCI request filter.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-filter">filter()</a> ###


<pre><code>
filter() = {eq, <a href="#type-key">key()</a>, <a href="#type-value">value()</a>} | {like, <a href="#type-key">key()</a>, <a href="#type-value">value()</a>}
</code></pre>




### <a name="type-key">key()</a> ###


<pre><code>
key() = binary() | '_'
</code></pre>




### <a name="type-t">t()</a> ###


<pre><code>
t() = [<a href="#type-filter">filter()</a>]
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_eq-3">add_eq/3</a></td><td>Add an equality filter.</td></tr><tr><td valign="top"><a href="#add_like-3">add_like/3</a></td><td>Add a like filter.</td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td>Return true if Object match filter, false otherwise.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Create new filter.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_eq-3"></a>

### add_eq/3 ###

<pre><code>
add_eq(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, Filters::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Add an equality filter

<a name="add_like-3"></a>

### add_like/3 ###

<pre><code>
add_like(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, Filters::<a href="#type-t">t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Add a like filter.
Value must be a string.

<a name="match-2"></a>

### match/2 ###

<pre><code>
match(Obj::<a href="occi_type.md#type-t">occi_type:t()</a>, Filter::<a href="#type-t">t()</a>) -&gt; boolean()
</code></pre>
<br />

Return true if Object match filter, false otherwise

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Create new filter

