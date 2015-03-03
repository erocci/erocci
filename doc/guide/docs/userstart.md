# Get It !

Get erocci sources from git repository:

```
git clone https://github.com/erocci/erocci.git
```

# Build It !

## Dependancies

erocci build system uses autotools, and some dependancies relies on
erlang specific build tool: rebar.

Dependancies are:

* automake (>= 1.11)
* autoconf (>= 2.69)
* rebar
* erlang/OTP (>= 16)

Optionally:

* libavahi_compat_libdns_sd headers: for XMPP listener

* libaprutil: for htpasswd authentication backend

* mkdocs: for building HTML documentation (available as markdown anyway)

## Build

* Prepare build system (only if you get sources from git):
```
./autogen.sh
```

* Configure
```
./configure
```

* Build
```
make
```

# Rock It !

TODO
