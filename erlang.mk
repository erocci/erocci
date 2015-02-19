# Copyright (c) 2015, Jean Parpaillon <jean.parpaillon@free.fr>
#
# Description:
#   Modified version of erlang.mk from Loïc Hoguin for using with erlang-mk.m4
#   and autotools
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

erlc_v = $(erlc_v_@AM_V@)
erlc_v_ = $(erlc_v_@AM_DEFAULT_V@)
erlc_v_0 = @echo "  ERLC    " $@;

xyrl_v = $(xyrl_v_@AM_V@)
xyrl_v_ = $(xyrl_v_@AM_DEFAULT_V@)
xyrl_v_0 = @echo "  XYRL    " $@;

all-local: all-erlang
clean-local: clean-erlang
dist-hook: dist-erlang

esrcdir = $(srcdir)/src
ebindir = $(srcdir)/ebin
appdata = $(addprefix $(ebindir)/,$(addsuffix .app,$(erlang_APPS)))
appbins = $(addprefix $(ebindir)/,$(addsuffix .beam,$(foreach app,$(erlang_APPS),$($(app)_MODULES))))

all-erlang: $(appdata) $(appbins)

$(ebindir)/%.app: $(esrcdir)/%.app.in $(top_srcdir)/config.status
	@$(MKDIR_P) $(@D)
	$(AM_V_GEN)$(top_srcdir)/config.status --file=$(esrcdir)/$(@F) > /dev/null
	@mv $(esrcdir)/$(@F) $@

$(ebindir)/%.beam: $(esrcdir)/%.erl
	@$(MKDIR_P) $(@D)
	$(erlc_v)$(ERLC) $(ERLCFLAGS) -o $(@D) $<

$(esrcdir)/%.erl: $(esrcdir)/%.xrl
	$(xyrl_v)$(ERLC) -o $(<D) $<

clean-erlang:
	-rm -rf $(appdata)
	-rm -rf $(appbins)
	-for base in $(basename $(wildcard $(esrcdir)/*.erl)); do \
	  if test -e $$base.xrl -o -e $$base.yrl; then rm -f $$base.erl; fi; \
	done

dist-erlang:
	@for file in $(wildcard $(addsuffix .in,$(addprefix $(esrcdir)/,$(erlang_APPS)))) \
		$(foreach mod,$(addprefix, $(esrcdir)/,$(foreach app,$(erlang_APPS),$($(app)_MODULES))), \
		   $(if $(wildcard $(mod).xrl), \
		      $(mod).xrl), \
		      $(if (wildcard $(mod).yrl), \
		         $(mod).yrl), \
		         $(mod).erl); do \
	  $(MKDIR_P) $(dirname $$file); \
	  cp $$file $(distdir)/$$file; \
	done

deps:
	$(MAKE) fetch-deps
	$(MAKE) build-deps

fetch-deps:
	@mkdir -p $(erlangdepsdir)
	for dep in $(erlang_DEPS); do \
	  VCS=$(dep)_VCS; \
	  URL=$(dep)_URL; \
	  VERSION=$(dep)_VERSION; \
	  if [ "$$$$VCS" = "git" ]; then \
	    if [ ! -d $(erlangdepsdir)/$(dep)/.git ]; then \
	      git clone -n -- $$$$URL $(erlangdepsdir)/$(dep); \
	    fi; \
	    cd $(erlangdepsdir)/$(1) && git checkout -q $$$$VERSION; \
	  elif [ "$$$$VCS" = "hg" ]; then \
	    if [ ! -d $(erlangdepsdir)/$(dep)/.hg ]; then \
	      hg clone -U $$$$URL $(erlangdepsdir)/$(dep); \
	    fi; \
	    cd $(erlangdepsdir)/$(1) && hg update -q $$$$VERSION; \
	  elif [ "$$$$VCS" = "svn" ]; then \
	    svn checkout $$$$URL $(erlangdepsdir)/$(dep); \
	  else \
	    echo "Unknown or invalid dependency: $(dep)." >&2; \
	    exit 78; \
	  fi; \
	done

build-deps: fetch-deps
	@for dep in $(erlang_DEPS) ; do \
	    if [ -f $$dep/GNUmakefile ] || [ -f $$dep/makefile ] || [ -f $$dep/Makefile ]; then \
	        $(MAKE) -C $$dep ; \
	    elif [ -f $$dep/rebar.config -a -n "$(REBAR)" ]; then \
	        ( cd $$dep && $(REBAR) compile ); \
	    else \
	        echo "Can not build dependancy: $$dep" ; \
	    fi ; \
	done

.PHONY: deps fetch-deps build-deps
