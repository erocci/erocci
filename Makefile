version = 1.0

PROJECT = erocci
PROJECT_VERSION = $(shell git describe --always --tags 2> /dev/null | sed -e 's/v\(.*\)/\1/' || echo $(version))

DEPS = \
	erocci_core \
	erocci_listener_http \
	erocci_backend_mnesia \
	erocci_backend_dbus \
	erocci_frontend \
	edown

TEST_DEPS = pocci

EDOC_OPTS = {app_default, "http://www.erlang.org/doc/"} \
           ,{doclet, edown_doclet} \
           ,{top_level_readme, {"$(CURDIR)/doc/README.md", "http://github.com/erocci/erocci"}}
EDOC_SRC_DIRS = \
	$(ALL_APPS_DIRS) \
	$(DEPS_DIR)/occi \
	$(DEPS_DIR)/erocci_core \
	$(DEPS_DIR)/erocci_listener_http \
	$(DEPS_DIR)/erocci_backend_mnesia \
	$(DEPS_DIR)/erocci_backend_dbus

LOCK = deps.lock
include $(LOCK)

POCCI_DATA = $(TEST_DIR)/pocci_SUITE_data/pocci.conf

dep_erocci_core = git https://github.com/erocci/erocci_core.git $(erocci_core_v)
dep_erocci_listener_http = git https://github.com/erocci/erocci_listener_http.git $(erocci_listener_http_v)
dep_erocci_backend_mnesia = git https://github.com/erocci/erocci_backend_mnesia.git $(erocci_backend_mnesia_v)
dep_erocci_backend_dbus = git https://github.com/erocci/erocci_backend_dbus.git $(erocci_backend_dbus_v)
dep_pocci = git https://github.com/jeanparpaillon/pOCCI.git $(pocci_v)
dep_erocci_frontend = git https://github.com/occiware/OCCInterface.git master

include erlang.mk

# TODO: use double-quote instead of single + atom_to_list
#       The problem is in correctly escaping double-quotes
define edoc.erl
	SrcPaths = lists:foldl(fun (P, Acc) ->
	                         filelib:wildcard(atom_to_list(P) ++ "/{src,c_src}") ++ Acc
	                       end, [], [$(call comma_list,$(patsubst %,'%',$(EDOC_SRC_DIRS)))]),
	DefaultOpts = [ {source_path, SrcPaths}
	               ,{subpackages, false} ],
	edoc:application($(1), ".", [$(2)] ++ DefaultOpts),
	halt(0).
endef

clean:: clean-local

clean-local:
	rm -f $(POCCI_DATA)

fulldoc: doc-deps
	$(gen_verbose) $(call erlang,$(call edoc.erl,$(PROJECT),$(EDOC_OPTS)))

test-build:: $(POCCI_DATA)

$(POCCI_DATA):
	@echo "{'POCCI', \"$(DEPS_DIR)/pocci/pOCCI/pOCCI.py\"}." > $@

lock: deps
	@rm -f $(LOCK)
	@for dep in occi $(DEPS) $(TEST_DEPS); do \
	  v=$$(cd deps/$${dep} && git describe --always); \
	  echo "$${dep}_v = $${v}" | tee --append $(LOCK); \
	done

.PHONY: fulldoc lock clean-local
