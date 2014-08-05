#!/usr/bin/env python
#
# @copyright (c) 2014 Jean Parpaillon
# @author Jean Parpaillon <jean.parpaillon@free.fr>
#
import os
import sys

from gi.repository import Gtk
import dbus
import dbus.service
from dbus.mainloop.glib import DBusGMainLoop

TYPE_UNDEFINED      = 0
TYPE_CAPABILITIES   = 1
TYPE_RESOURCE       = 2
TYPE_LINK           = 3
TYPE_BOUNDED_COLL   = 4
TYPE_UNBOUNDED_COLL = 5
TYPE_MIXIN          = 6

BACKEND_IFACE = 'org.ow2.erocci.backend'

SIG_NODE = '(svvsyv)'

class OcciNode(object):

    @classmethod
    def from_dbus(cls, (_id, objid, owner, etag, _type, content)):
        cls(_id,
            from_objid(_type, objid),
            owner,
            _type,
            etag,
            from_content(_type, content)
        )

    def __init__(self, _id, objid, owner, _type, etag, data):
        self._id = _id
        self._objid = objid
        self._owner = owner
        self._type = _type
        self._etag = etag
        self._data = data

    def to_dbus(self):
        return (
            dbus.types.String(self._id),
            to_objid(self._type, self._objid),
            dbus.types.String(self._owner),
            dbus.types.String(self._etag),
            dbus.types.Byte(self._type),
            to_content(self._type, self._data)
        )


class OcciCategory():

    @classmethod
    def from_dbus(cls, scheme, term):
        return cls(scheme, term)

    def __init__(self, scheme, term):
        self._scheme
        self._term

    def to_dbus(self):
        return ( dbus.types.String(self._scheme), dbus.types.String(self._term) )


class OcciMixin(object):

    @classmethod
    def from_dbus(cid, url):
        return cls(cid, url)

    def __init__(self, cid, url):
        self._cid = cid
        self._url = url

    def to_dbus(self):
        return ( dbus.types.Struct(cid.to_dbus(), 1), self._url )

    
class OcciResource(object):

    @classmethod
    def from_dbus(cls, _id, kind, mixins=[], attrs=[], links=[]):
        return cls(from_objid(TYPE_RESOURCE, _id),
                   OcciCategory.from_dbus(kind),
                   map(lambda m: OcciCategory.from_dbus(m), mixins),
                   reduce(lambda a, acc: from_attr(a, acc), attrs),
                   map(lambda l: from_resource_link(l), links))

    def __init__(self, _id, kind, mixins=[], attributes=[], links=[]):
        self._id = _id
        self._kind = kind
        self._mixins = mixins
        self._attributes = attributes
        self._links = links

    def to_dbus(self):
        return ( dbus.types.Struct(to_objid(self._id), 1),
                 self._kind.to_dbus(),
                 dbus.types.Array(map(lambda m: m.to_dbus(), self._mixins), '((ss)s)'),
                 dbus.types.Dictionary(map(lambda a: to_attr(a), self._attrs.items()), 'sv'),
                 dbus.types.Array(map(lambda l: to_resource_link(l), self._links), 'v') )


class OcciLink(object):

    @staticmethod
    def from_dbus(cls, _id, kind, mixins, src, target, target_cid, attrs):
        return cls( from_objid(TYPE_LINK, _id),
                    OcciCategory.from_dbus(kind),
                    map(lambda m: OcciCategory.from_dbus(m), mixins),
                    src,
                    target,
                    OcciCategory.from_dbus(target_cid),
                    reduce(lambda a, acc: from_attr(a, acc), attrs))

    def __init__(self, _id, kind, mixins, src, target, target_cid, attributes):
        self._id = _id
        self._kind = kind
        self._mixins = mixins
        self._src = src
        self._target = target
        self._target_cid = target_cid
        self._attributes = attributes

    def to_dbus(self):
        return ( dbus.types.Struct(to_objid(self._objid, 1)),
                 self._kind.to_dbus(),
                 dbus.types.Array(map(lambda m: m.to_dbus(), self._mixins), '((ss)s)'),
                 dbus.types.String(self._src),
                 dbus.types.String(self._target),
                 self._target_cid.to_dbus(),
                 dbus.types.Dictionary(map(lambda a: to_attr(a), self._attrs.items()), 'sv') )


class OcciCollection(object):

    @staticmethod
    def from_dbus(cls, (_id, uris)):
        if isinstance(_id, tuple):
            return cls( from_objid(TYPE_BOUNDED_COLL, _id), uris )
    
    def __init__(self, _id, uris):
        self._id = _id
        self._uris = uris

    def to_dbus(self):
        return ( dbus.types.Struct(to_objid(self._objid), 1),
                 dbus.types.Array(self._uris, 's') )


def from_objid(_type, var):
    if var == False:
        return False
    elif _type == TYPE_CAPABILITIES:
        return OcciCategory(var[0], var[1])
    elif _type == TYPE_BOUNDED_COLL:
        return OcciCategory(var[0], var[1])
    return var


def from_content(_type, var):
    if _type == TYPE_UNDEFINED:
        return False
    elif _type == TYPE_CAPABILITIES:
        return reduce(lambda m: OcciMixin(m[0][0], m[0][1], m[2]), var)
    elif _type == TYPE_MIXINX:
        return OcciMixin(var[0][0], var[0][1], var[2])
    elif _type == TYPE_RESOURCE:
        return OcciResource(var[0], var[1], var[2], var[3], var[4])
    elif _type == TYPE_LINK:
        return OcciLink(var[0], var[1], var[2], var[3], var[4], var[5], var[6])
    elif _type == TYPE_BOUNDED_COLL:
        return OcciCollection(var[0], var[1])
    elif _type == TYPE_UNBOUNDED_COLL:
        return OcciCollection(var[0], var[1])

    
def from_attr(attr, acc):
    acc[attr[0]] = attr[1]
    return acc


def to_objid(_type, objid):
    if objid == False:
        return False
    elif _type == TYPE_CAPABILITIES:
        return dbus.Struct(objid.to_dbus(), 1)
    elif _type == TYPE_BOUNDED_COLL:
        return dbus.Struct(objid.to_dbus(), 1)
    return objid


def to_content(_type, data):
    if data == False:
        return False
    elif _type == TYPE_CAPABILITIES:
        return dbus.types.Array(map(lambda m: m.to_dbus(), data), '((ss)s)', 1)
    return dbus.types.Struct(data.to_dbus(), 1)


def to_attr(attr):
    if isinstance(attr[0], float):
        return ( attr[0], dbus.types.Double(attr[1]) )
    elif isinstance(attr[1], int):
        return ( attr[0], dbus.types.Int64(attr[1]) )
    return ( attr[0], dbus.types.String(attr[1]) )


def to_resource_links(link):
    if isinstance(link, OcciLink):
        return link.to_dbus()
    return dbus.types.String(link)
