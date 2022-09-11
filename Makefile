# Copyright (c) 2022 Free Software Foundation, Inc.

# Author: Simon Dobson <simoninireland@gmail.com>
# Maintainer: Simon Dobson <simoninireland@gmail.com>
# Keywords: hypermedia, attachments

# This file is NOT part of GNU Emacs.
#
# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

# Source files
SOURCES_FILES = \
	ox-attach-publish.el \
	ox-attach-publish-f.el \
	ox-attach-publish-machinery.el \
	ox-attach-publish-frontend.el

# Unit tests
SOURCES_TESTS =\
	test/test-f.el \
	test/test-machinery.el \
	test/test-frontend.el
INIT_TESTS = test/init.el

# Tools
EMACS = emacs
CASK = cask
RM = rm -fr

# Virtual environment
VENV = .cask

# Constructed tools
RUN_EMACS = $(CASK) exec $(EMACS) -Q -batch -L "."

# Top-level targets

# Build virtual environment
.PHONY: env
env: $(VENV)

$(VENV):
	$(CASK) install

.PHONY: test
test: env
	$(RUN_EMACS) \
	-l $(INIT_TESTS) \
	$(SOURCES_TESTS:%=-l %) \
	--eval "(let ((ert-quiet t)) (ert-run-tests-batch-and-exit))"

.PHONY: lint
lint: env
	$(RUN_EMACS) \
	--eval "(progn (require 'package-lint)(package-lint-batch-and-exit))" \
	$(SOURCES_FILES)

# Clean up the build
.PHONY: clean
clean:
	$(RM) $(VENV)
