;;; test-f.el -- Test list and file utilities  -*- lexical-binding: t -*-

;; Copyright (c) 2022 Simon Dobson <simoninireland@gmail.com>

;; Author: Simon Dobson <simoninireland@gmail.com>
;; Maintainer: Simon Dobson <simoninireland@gmail.com>
;; Homepage: https://github.com/simoninireland/ox-attach-publish

;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Test list and file utilities.
;; Uses some ideas taken from https://github.com/jkitchin/org-ref/blob/master/test/init.el
;; using a macro to define a local set o publishing projects to allow the tests to be
;; run inside a development Emacs without screwing-up the main settings (hurray for
;; dynamic scoping!).
;;
;; Assumes that cask or similar has been used to create an approproate virtual environment.

;;; Code:

(require 'ox-attach-publish)

(defmacro with-test-projects (&rest body)
  "Run BODY forms in a local context with publishing projects and their directories.

The variables 'content-dir', 'static-dir', and 'publish-dir' correspond
to the base directories for the \"content\" and \"static\" projects, and
the the overall publishing directory, respectively. 'publish-dir' is
guaranteed to exist and be empty when BODY runs."
  `(let* ((content-dir (expand-file-name "test/content"))
	  (static-dir (expand-file-name "test/static"))
	  (publish-dir (expand-file-name "test/www"))
	  (attachments-subdir "attachments")
	  (content-publish-dir publish-dir)
	  (static-publish-dir publish-dir))
     (when (f-directory-p publish-dir)
       (delete-directory publish-dir t))
     (make-directory publish-dir)
     (let ((org-publish-project-alist
	    `(("content"
	       :base-directory ,content-dir
	       :publishing-directory ,content-publish-dir
	       :base-extension "org"
	       :recursive t
	       ;; :publishing-function org-html-publish-to-html
	       :publishing-function org-attach-publish-to-html
	       :attachments-project "static"
	       :attachments-base-directory ,attachments-subdir)
	      ("static"
	       :base-directory ,static-dir
	       :publishing-directory ,static-publish-dir
	       :base-extension "png\\|txt"
	       :recursive t
	       :publishing-function org-publish-attachment))))
       (unwind-protect
	   (progn
	     ,@body)
	 (delete-directory publish-dir t)))))
