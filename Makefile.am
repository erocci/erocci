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
SUBDIRS = apps doc test

erlang_APP = occi
erlang_MODULES = occi_app occi occi_log_handler
erlang_PRIV = \
	configs \
	htpasswd \
	schemas \
	scripts \
	ssl

include $(top_srcdir)/erlang.mk

edepsdir = $(top_builddir)/deps
edepsdirs = $(addprefix $(edepsdir)/,$(erlang_DEPS))

# For compat with erlang.mk
DEPS_DIR = $(abspath $(edepsdir))
export DEPS_DIR

# For compat with rebar
REBAR_DEPS_DIR = $(abspath $(edepsdir))
export REBAR_DEPS_DIR

if ERL_DEPS
all-local: all-erlang-deps
install-data-local: install-erlang-deps
uninstall-local: uninstall-erlang-deps
clean-local: clean-erlang-deps
dist-hook: dist-deps
endif

MAKE_OPTS = 
if LIBXML2
MAKE_OPTS += "HAVE_LIBXML2=yes"
endif
if HAVE_EXPAT
MAKE_OPTS += "HAVE_EXPAT=yes"
endif

ERLC_OPTS = $(AM_ERLCFLAGS)
export ERLC_OPTS

all-erlang-deps: deps
	@for dep in $(addprefix $(edepsdir)/,$(erlang_DEPS)) ; do \
	    if [ -f $$dep/GNUmakefile ] || [ -f $$dep/makefile ] || [ -f $$dep/Makefile ]; then \
	        $(MAKE) -C $$dep $(MAKE_OPTS); \
	    elif [ -f $$dep/rebar.config -a -n "$(REBAR)" ]; then \
	        ( cd $$dep && $(REBAR) compile ); \
	    fi ; \
	done

deps: $(edepsdirs)
	@for dep in $(addprefix $(edepsdir)/,$(erlang_DEPS)) ; do \
	    if [ -f $$dep/rebar.config -a -n "$(REBAR)" ]; then \
	        ( cd $$dep && $(REBAR) get-deps ); \
	    elif [ -f $$dep/erlang.mk ]; then \
	        $(MAKE) -C $$dep deps $(MAKE_OPTS); \
	    fi ; \
	done

test: deps
	$(MAKE) -C test test-ct

define dep_fetch
$(edepsdir)/$(1):
	@mkdir -p $(edepsdir)
	if [ "$($(1)_DEP_VCS)" = "git" ]; then \
	  if [ ! -d $(edepsdir)/$(1)/.git ]; then \
	    git clone -n -- $($(1)_DEP_URL) $(edepsdir)/$(1); \
	  fi; \
	  cd $(edepsdir)/$(1) && git checkout -q $($(1)_DEP_VER); \
	elif [ "$($(1)_DEP_VCS)" = "hg" ]; then \
	  if [ ! -d $(edepsdir)/$(1)/.hg ]; then \
	    hg clone -U $($(1)_DEP_URL) $(edepsdir)/$(1); \
	  fi; \
	  cd $(edepsdir)/$(1) && hg update -q $($(1)_DEP_VER); \
	elif [ "$($(1)_DEP_VCS)" = "svn" ]; then \
	  svn checkout $($(1)_DEP_URL) $(edepsdir)/$(1); \
	else \
	  echo "Unknown or invalid dependency: $(1)." >&2; \
	  exit 78; \
	fi
endef

$(foreach dep,$(erlang_DEPS),$(eval $(call dep_fetch,$(dep))))

install-erlang-deps:

uninstall-erlang-deps:

clean-erlang-deps:
	@for dep in $(addprefix $(edepsdir)/,$(erlang_DEPS)) ; do \
	    if [ -f $$dep/GNUmakefile ] || [ -f $$dep/makefile ] || [ -f $$dep/Makefile ]; then \
	        $(MAKE) -C $$dep clean; \
	    elif [ -f $$dep/rebar.config -a -n "$(REBAR)" ]; then \
	        ( cd $$dep && $(REBAR) clean ); \
	    fi ; \
	done

dist-deps: deps
	@mkdir -p $(distdir)/deps
	@for dep in $(erlang_DEPS); do \
	  if test -e $(builddir)/deps/$$dep; then \
	    $(MAKE) clean-erlang-deps; \
	    echo "  DISTDEP "$$dep; \
	    tar --exclude-vcs -cf - $(builddir)/deps/$$dep | tar -xf - -C $(distdir)/; \
	  fi; \
	done

.PHONY: all-erlang-deps install-erlang-deps uninstall-erlang-deps clean-erlang-deps dist-deps deps
