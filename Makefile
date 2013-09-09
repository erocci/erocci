topdir=.

ERL ?= erl
REBAR=$(topdir)/rebar

.PHONY: alll deps clean distclean docs exmpp-deps tests tests-all

all: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	find -name '*~' -exec rm {} \;
	rm -f erl_crash.dump
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

tests:
	@$(REBAR) eunit recursive=false skip_deps=true

tests-all:
	@$(REBAR) eunit
