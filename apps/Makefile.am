# Copyright 2015 Jean Parpaillon, all rights reserved
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
SUBDIRS = core authnz listener_http backend_mnesia

if LISTENER_XMPP
SUBDIRS += listener_xmpp
endif

if AUTHNZ_HTPASSWD
SUBDIRS += authnz_htpasswd
endif

if DBUS
SUBDIRS += backend_dbus
endif

if BACKEND_ADMIN
SUBDIRS += backend_admin
endif
