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

install_v = $(install_v_@AM_V@)
install_v_ = $(install_v_@AM_DEFAULT_V@)
install_v_0 = @echo "  INSTALL " $@;

rm_v = $(rm_v_@AM_V@)
rm_v_ = $(rm_v_@AM_DEFAULT_V@)
rm_v_0 = @echo "  INSTALL " $@;

all-local: all-erlang
install-data-local: install-erlang-app
uninstall-local: uninstall-erlang-app
clean-local: clean-erlang
dist-hook: dist-erlang

esrcdir = $(srcdir)/src
ebindir = $(builddir)/ebin
ecsrcdir = $(builddir)/c_src
eincludedir = $(srcdir)/include
eprivdir = $(srcdir)/priv

appdata = ebin/$(erlang_APP).app
appbins = $(addprefix $(ebindir)/,$(addsuffix .beam,$(foreach mod,$(erlang_MODULES),$(shell basename $(mod)))))
appfirst = $(addprefix $(ebindir)/,$(addsuffix .beam,$(foreach mod,$(erlang_FIRST),$(shell basename $(mod)))))
appports = $(addprefix $(eprivdir)/,$(addsuffix .so,$(erlang_PORTS)))

space := $(empty) $(empty)
comma := ,

edit = sed \
	-e 's|@ERL_APP@|'$(erlang_APP)'|g' \
	-e 's|@ERL_MODULES@|'$(subst $(space),$(comma),$(foreach mod,$(erlang_MODULES),$(shell basename $(mod))))'|'

###
### Build
###
all-erlang: $(appdata) $(appports)
	$(MAKE) all-first
	$(MAKE) all-beams

all-first: $(appfirst)

all-beams: $(filter-out $(appfirst),$(appbins))

ebin/%.app: src/%.app.in $(top_builddir)/config.status Makefile
	@$(MKDIR_P) $(@D)
	$(AM_V_GEN)$(top_builddir)/config.status --file=$@:$< > /dev/null; \
	  $(edit) $@ > $@.tmp; \
	  mv $@.tmp $@

define compile_erl
ebin/$(1).beam: src/$(2).erl
	@$(MKDIR_P) $(ebindir)
	$(erlc_v)$(ERLC) -pa $(ebindir) $(ERLCFLAGS) -o $(builddir)/ebin $$<
endef

define compile_xrl
ebin/$(1).beam: src/$(2).xrl
	@$(MKDIR_P) $(builddir)/$$(<D)
	$(xyrl_v)$(ERLC) -o $(builddir)/$$(<D) $$<
	$(erlc_v) \
	  erlsrc=`echo $$< | sed -e 's,\.xrl$$$$,.erl,'`; \
	  $(ERLC) -pa $(ebindir) $(ERLCFLAGS) -o $(builddir)/ebin $(builddir)/$$$$erlsrc; \
	  rm -f $(builddir)/$$$$erlsrc
endef

define compile_yrl
ebin/$(1).beam: src/$(2).yrl
	@$(MKDIR_P) $(builddir)/$$(<D)
	$(xyrl_v)$(ERLC) -o $(builddir)/$$(<D) $$<
	$(erlc_v) \
	  erlsrc=`echo $$< | sed -e 's,\.yrl$$$$,.erl,'`; \
	  $(ERLC) -pa $(ebindir) $(ERLCFLAGS) -o $(builddir)/ebin $(builddir)/$$$$erlsrc; \
	  rm -f $(builddir)/$$$$erlsrc
endef

erlmod = $(foreach mod,$(erlang_MODULES),$(if $(wildcard $(srcdir)/src/$(mod).erl),$(mod)))
xrlmod = $(foreach mod,$(erlang_MODULES),$(if $(wildcard $(srcdir)/src/$(mod).xrl),$(mod)))
yrlmod = $(foreach mod,$(erlang_MODULES),$(if $(wildcard $(srcdir)/src/$(mod).yrl),$(mod)))

$(foreach mod,$(erlmod), $(eval $(call compile_erl,$(shell basename $(mod)),$(mod))))
$(foreach mod,$(xrlmod), $(eval $(call compile_xrl,$(shell basename $(mod)),$(mod))))
$(foreach mod,$(yrlmod), $(eval $(call compile_yrl,$(shell basename $(mod)),$(mod))))

define build_port
priv/$(1).so: $(2)
	cp -fp $(ecsrcdir)/.libs/$$(@F) $$@

c_src/%.la:
	$(MAKE) -C $(@D) $(@F)
endef

$(foreach port,$(erlang_PORTS),$(eval $(call build_port,$(port),$(ecsrcdir)/$(port).la)))

###
### Install / Uninstall
###
install-erlang-app:
	@$(MKDIR_P) $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_$(erlang_APP))/ebin
	@for beam in $(foreach mod,$(erlang_MODULES),$(shell basename $(mod)).beam); do \
	  $(INSTALL_DATA) $(ebindir)/$$beam $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_$(erlang_APP))/ebin/$$beam; \
	done
	$(INSTALL_DATA) $(appdata) $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_$(erlang_APP))/ebin/$(erlang_APP).app
	@$(MKDIR_P) $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_$(erlang_APP))/include
	@for hrl in $(erlang_HRL); do \
	  $(INSTALL_DATA) $(eincludedir)/$$hrl.hrl $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_$(erlang_APP))/include/$$hrl.hrl; \
	done
	@$(MKDIR_P) $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_$(erlang_APP))/priv
	@for data in $(erlang_PRIV); do \
	  cp -fpR $(eprivdir)/$$data $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_$(erlang_APP))/priv/$$data; \
	done

uninstall-erlang-app:
	@for beam in $(foreach mod,$(erlang_MODULES),$(shell basename $(mod)).beam); do \
	  rm -f $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_$(erlang_APP))/ebin/$$beam; \
	done
	@for hrl in $(erlang_HRL); do \
	  rm -f $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_$(erlang_APP))/include/$$hrl.hrl; \
	done
	@for data in $(erlang_PRIV); do \
	  rm -rf $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_$(erlang_APP))/priv/$$data; \
	done

###
### Clean
###
clean-erlang:
	-rm -rf $(appbins) $(appdata)
	-for base in $(shell find $(srcdir)/src -name '*.xrl'); do \
	  gen=`echo $$base | sed -e 's,\.xrl$$,.erl,'`; \
	  if test -e $$gen; then rm -f $$gen; fi; \
	done
	-for base in $(shell find $(srcdir)/src -name '*.yrl'); do \
	  gen=`echo $$base | sed -e 's,\.yrl$$,.erl,'`; \
	  if test -e $$gen; then rm -f $$gen; fi; \
	done

###
### Dist
###
dist-erlang:
	@for file in  $(wildcard $(esrcdir)/$(erlang_APP).app.in) \
	        $(addprefix $(eincludedir)/,$(addsuffix .hrl,$(erlang_HRL))) \
		$(foreach mod,$(addprefix $(esrcdir)/,$(erlang_MODULES)), \
	           $(if $(wildcard $(mod).xrl), \
	              $(mod).xrl, \
	              $(if $(wildcard $(mod).yrl), \
	                 $(mod).yrl, \
	                 $(mod).erl))); do \
	  dirname=`echo $$file | sed -e 's,/*[^/]\+/*$$,,'`; \
	  $(MKDIR_P) $(distdir)/$$dirname; \
	  cp $$file $(distdir)/$$file; \
	done

.PHONY: all-erlang all-first all-beams clean-erlang dist-erlang install-erlang-app uninstall-erlang-app