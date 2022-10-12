;;; test-machinery.el --- Backend tests -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Free Software Foundation, Inc.

;; Author: Simon Dobson <simoninireland@gmail.com>
;; Maintainer: Simon Dobson <simoninireland@gmail.com>
;; Keywords: hypermedia, attachments

;; This file is NOT part of GNU Emacs.

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

;; Test the backend functions.

;;; Code:

(require 'ert)


;; ---------- Project names ----------

(ert-deftest project-from-project-name ()
  (with-test-projects
   (should (let* ((proj "content")
		  (info (org-attach-publish--project-name-or-plist proj)))
	     (equal (plist-get info :recursive)
		    t)))))

(ert-deftest project-from-project ()
  (with-test-projects
   (should (let* ((proj (assoc "content" org-publish-project-alist))
		  (info (org-attach-publish--project-name-or-plist proj)))
	     (equal (plist-get info :recursive)
		    t)))))

(ert-deftest project-from-project-plist ()
  (with-test-projects
   (should (let* ((proj (cdr (assoc "content" org-publish-project-alist)))
		  (info (org-attach-publish--project-name-or-plist proj)))
	     (equal proj info)))))


;; ---------- Accessing bits of projects ----------

(ert-deftest content-base-dir ()
  (with-test-projects
   (should (equal (org-attach-publish--attachments-base-dir "content")
		  (concat static-dir (f-path-separator) attachments-subdir)))))

(ert-deftest content-base-dir ()
  (with-test-projects
   (should (equal (org-attach-publish--attachments-pub-dir "content")
		  (concat static-publish-dir (f-path-separator) attachments-subdir)))))


;; ---------- Relative links ----------

;; ---------- Link re-writing ----------

(ert-deftest rewrite-link ()
  (with-test-projects
   (let* ((fn (concat publish-dir "/a/b/c.html"))
	  (attachment "ttt.gif")
	  (uid-dir "12/345")
	  (attachment-dir (concat publish-dir "/" attachments-subdir "/" uid-dir)))
     (should (equal (org-attach-publish--rewrite-link (org-attach-publish--split-path fn)
						      (org-attach-publish--split-path attachment)
						      (org-attach-publish--split-path attachment-dir))
		    (concat "../../" attachments-subdir "/" uid-dir "/" attachment))))))
