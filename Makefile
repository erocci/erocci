version = 1.0

PROJECT = erocci
PROJECT_VERSION = $(shell git describe --always --tags 2> /dev/null | sed -e 's/v\(.*\)/\1/' || echo $(version))

DEPS = \
	erocci_core \
	erocci_authnz \
	erocci_listener_http \
	erocci_backend_mnesia \
	edown

EDOC_OPTS = {app_default, "http://www.erlang.org/doc/"} \
           ,{doclet, edown_doclet} \
           ,{top_level_readme, {"$(CURDIR)/README.md", "http://github.com/erocci/erocci"}}
EDOC_SRC_DIRS = \
	$(ALL_APPS_DIRS) \
	$(DEPS_DIR)/occi \
	$(DEPS_DIR)/erocci_core \
	$(DEPS_DIR)/erocci_authnz \
	$(DEPS_DIR)/erocci_listener_http \
	$(DEPS_DIR)/erocci_backend_mnesia

LOCK = deps.lock

dep_erocci_core = git https://github.com/erocci/erocci_core.git $(erocci_core_v)
dep_erocci_authnz = git https://github.com/erocci/erocci_authnz.git $(erocci_authnz_v)
dep_erocci_listener_http = git https://github.com/erocci/erocci_listener_http.git $(erocci_listener_http)
dep_erocci_backend_mnesia = git https://github.com/erocci/erocci_backend_mnesia.git $(erocci_backend_mnesia)
dep_erocci_backend_dbus = git https://github.com/erocci/erocci_backend_dbus.git $(erocci_backend_dbus)

include erlang.mk

lock: deps
	@rm -f $(LOCK)
	@for dep in $(DEPS); do \
	  v=$$(cd deps/$${dep} && git describe --always); \
	  echo "$${dep}_v = $${v}" | tee --append $(LOCK); \
	done

.PHONY: lock 
