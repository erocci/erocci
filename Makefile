topdir=.

ERL ?= erl
REBAR=$(topdir)/rebar

.PHONY: deps exmpp-deps

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
