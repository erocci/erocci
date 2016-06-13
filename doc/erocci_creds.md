

# Module erocci_creds #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Represents authentication credentials.

Copyright (c) (C) 2016, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-group">group()</a> ###


<pre><code>
group() = binary() | admin | anonymous
</code></pre>




### <a name="type-t">t()</a> ###


<pre><code>
t() = #creds{}
</code></pre>




### <a name="type-type">type()</a> ###


<pre><code>
type() = basic | anonymous
</code></pre>




### <a name="type-user">user()</a> ###


<pre><code>
user() = binary() | admin | anonymous
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#anonymous-0">anonymous/0</a></td><td>Anonymous authentication.</td></tr><tr><td valign="top"><a href="#basic-1">basic/1</a></td><td>Basic credentials, without authentication.</td></tr><tr><td valign="top"><a href="#basic-3">basic/3</a></td><td>Basic authentication, with authentication
WARNING: implements hard coded user/password.</td></tr><tr><td valign="top"><a href="#challenge-1">challenge/1</a></td><td>Returns a challenge for authentication.</td></tr><tr><td valign="top"><a href="#group-1">group/1</a></td><td>Get request group.</td></tr><tr><td valign="top"><a href="#is_authenticated-1">is_authenticated/1</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td>Authentication type.</td></tr><tr><td valign="top"><a href="#user-1">user/1</a></td><td>Get request user.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="anonymous-0"></a>

### anonymous/0 ###

<pre><code>
anonymous() -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Anonymous authentication

<a name="basic-1"></a>

### basic/1 ###

<pre><code>
basic(Challenge::function()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Basic credentials, without authentication

<a name="basic-3"></a>

### basic/3 ###

<pre><code>
basic(User::<a href="#type-user">user()</a>, Password::binary(), Challenge::function()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Basic authentication, with authentication
WARNING: implements hard coded user/password

<a name="challenge-1"></a>

### challenge/1 ###

<pre><code>
challenge(Creds::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

Returns a challenge for authentication

<a name="group-1"></a>

### group/1 ###

<pre><code>
group(Creds::<a href="#type-t">t()</a>) -&gt; <a href="#type-group">group()</a>
</code></pre>
<br />

Get request group

<a name="is_authenticated-1"></a>

### is_authenticated/1 ###

<pre><code>
is_authenticated(Creds::<a href="#type-t">t()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(Creds::<a href="#type-t">t()</a>) -&gt; <a href="#type-type">type()</a>
</code></pre>
<br />

Authentication type

<a name="user-1"></a>

### user/1 ###

<pre><code>
user(Creds::<a href="#type-t">t()</a>) -&gt; <a href="#type-user">user()</a>
</code></pre>
<br />

Get request user

