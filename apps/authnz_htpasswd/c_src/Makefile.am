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
htpasswdprivdir = $(ERLANG_INSTALL_LIB_DIR_occi_authnz_htpasswd)/priv

htpasswdpriv_LTLIBRARIES = htpasswd.la
htpasswd_la_SOURCES = htpasswd.c
htpasswd_la_CFLAGS = $(ERL_CFLAGS) $(APRUTIL_CFLAGS)
htpasswd_la_LDFLAGS = $(ERL_LDFLAGS) $(APRUTIL_LDFLAGS) -module -avoid-version
