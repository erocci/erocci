# erocci User Guide

erocci is framework for building RESTful applications.

REST applications being composed of resources, designing an
application with erocci consists of declaring resources' categories.

Data can be stored and retrieved through various 'backends'
(databases, external APIs, etc). Backends can be written in erlang or
with a D-Bus API.

API can be exposed through HTTP but also XMPP. Thanks to the
compliance with [OCCI](occi.md) standard, API can be
easily described and discovered.

erocci is written in erlang and built on top of OTP platform and
cowboy web server.

## Index

### Introduction

* [Rationale](rationale.md)
* [The Open Cloud Computing Interface](occi.md)
* [Architecture](architecture.md)

### User Guide

* [erlang Basics](userintro.md)
* [Getting Started](userstart.md)
* [Advanced Configuration](userconf.md)

### Developper Guide

* [Listener API](devlistener.md)
* [Backend API](devbackend.md)
* [Authnz API](devauthnz.md)