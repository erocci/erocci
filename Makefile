topdir=.

ERL ?= erl
REBAR=$(shell which rebar || echo "echo \"rebar is missing. Exiting...\" && false")
APP=occi

.PHONY: all deps clean distclean doc doc-clean exmpp-deps tests tests-all

all: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean: doc-clean
	find -name '*~' -exec rm {} \;
	rm -f erl_crash.dump
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

doc:
	@$(REBAR) skip_deps=true doc

doc-clean:
	rm -f doc/*.html doc/edoc-info doc/erlang.png doc/stylesheet.css

tests:
	@$(REBAR) eunit recursive=false skip_deps=true

tests-all:
	@$(REBAR) eunit
