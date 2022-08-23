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
	test/test-f.el
INIT_TESTS = test/init.el

# Tools
EMACS = emacs
CASK = cask

.PHONY: test
test:
	${CASK} exec ${EMACS} -Q -batch -L "." -l ${INIT_TESTS} -l "test/test-f.el" --eval "(let ((ert-quiet t)) (ert-run-tests-batch-and-exit))"
