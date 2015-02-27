## Yet Another REST Framework ?

REST architecture style is widely used in modern distributed
applications. The style has been extensively described in Roy Fielding
[dissertation](https://www.ics.uci.edu/~fielding/pubs/dissertation/fielding_dissertation.pdf).

In a few words, REST architecture style is particularly suited for
interned-based applications, meaning unreliable and heterogeneous
networks (in term of latency, bandwidth, QoS, etc).

Nevertheless, REST is just an architecture and designing an RESTful
application means describing not only resources and resources types,
but also search syntax, URL mapping, etc. Would HTTP not be the right
protocol for your needs (bi-directional communication, binary-based
protocol, etc), REST architecture is simply not available.

Thanks to erocci and the Open Cloud Computing Interface
[OCCI](occi.md) it implements, designing a RESTful application just
means describing resources' types (called categories), then choosing
one or several backends (database, external API, etc) and one or
several listeners (ie HTTP, XMPP, etc).

## Implementation Choices

How erlang/OTP dovetails with a REST framework constraints is properly
described in [cowboy web server user
guide](http://ninenines.eu/docs/en/cowboy/HEAD/guide/erlang_web/).
