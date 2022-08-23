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
;; to create a clean environment, and a macro to define a local set of
;; publishing projects to allow the tests to be run ionside a development
;; Emacs without screwing-up the main settings (hurray for dynamic scoping!).

;;; Code:

;; ---------- Set up clean package directory ----------

(setq user-emacs-directory "./elpa-for-ox-attach-publish")
(require 'package)

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)


;; ---------- Install dependencies ----------

(dolist (package (list 'org 's 'f))
  (unless (package-installed-p package)
    (message "installing %s" package)
    (package-install package)))


;;---------- Test publishing projects ----------

(defmacro with-test-projects (&rest body)
  "Run BODY forms in a local context with publishing projects and their directories.

The variables 'content-dir', 'static-dir', and 'publish-dir' correspond
to the base directories for the \"content\" and \"static\" projects, and
the the overall publishing directory, respectively. These directories
are guaranteed to exist, and guaranteed to be empty, when BODY is run."
  `(let* ((content-dir "./.content")
	  (static-dir "./.static")
	  (publish-dir "./.www")
	  (attachments-subdir "attachments")
	  (content-publish-dir publish-dir)
	  (static-publish-dir (concat publish-dir "/static")))
     (dolist (dir (list content-dir static-dir publish-dir))
       (when (f-directory-p dir)
	 (delete-directory dir t))
       (make-directory dir))
     (let ((org-publish-project-alist
	    `(("content"
	       :base-directory ,content-dir
	       :publishing-directory ,content-publish-dir
	       :base-extension "org"
	       :recursive t
	       :publishing-function org-html-publish-to-html
	       :attachments-project "static"
	       :attachments-base-directory ,attachments-subdir)
	      ("static"
	       :base-directory ,static-dir
	       :publishing-directory ,static-publish-dir
	       :base-extension "jpeg\\|jpg\\|png\\|svg\\|pdf"
	       :recursive t
	       :publishing-function org-publish-attachment))))
       (unwind-protect
	   (progn
	     ,@body)
	 (dolist (dir (list content-dir static-dir publish-dir))
	   (delete-directory dir t))))))


(require 'ox-attach-publish)
