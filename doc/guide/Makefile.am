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
SOURCES = \
	docs/architecture.md \
	docs/devauthnz.md \
	docs/devbackend.md \
	docs/devlistener.md \
	docs/favicon.png \
	docs/index.md \
	docs/occi.md \
	docs/rationale.md \
	docs/userconf.md \
	docs/userintro.md \
	docs/userstart.md
DOCBUILD = $(builddir)/html
DOCSRC = $(srcdir)/docs

if BUILDDOC
all-local: doc-html
install-data-local: install-doc-html
uninstall-local: uninstall-doc-html
clean-local: clean-doc-html
endif

edit = sed -e 's|@DOCBUILD@|'$(DOCBUILD)'|' \
	   -e 's|@DOCSRC@|'$(DOCSRC)'|'

doc-html: $(SOURCES) mkdocs.yml
	$(MKDOCS) build --clean

mkdocs.yml: mkdocs.yml.in Makefile
	$(edit) $< > $@

install-doc-html: doc-html
	$(MKDIR_P) $(DESTDIR)$(docdir)
	cp -fR $(DOCBUILD) $(DESTDIR)$(docdir)/

uninstall-doc-html:
	-rm -rf $(DESTDIR)$(docdir)/html

clean-doc-html:
	-rm -rf $(DOCBUILD)
	-rm -f mkdocs.yml

EXTRA_DIST = $(SOURCES) mkdocs.yml.in docs/img

.PHONY: doc-html install-doc-html
