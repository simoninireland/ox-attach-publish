;;; ox-attach-publish-frontend.el -- Attachment publishing frontend  -*- lexical-binding: t -*-
;; Copyright (c) 2022 Free Software Foundation, Inc.

;; Author: Simon Dobson <simoninireland@gmail.com>
;; Maintainer: Simon Dobson <simoninireland@gmail.com>
;; Keywords: hypermedia, attachments

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

;; This is the user-facing front-end code for publishing attachments.

;;; Code:

(require 'f)


;; ---------- Project access ----------

(defun org-attach-publish--project-name-or-plist (proj)
  "Return the plist of PROJ, which may be the plist or a key into
'org-publish-project-alist'."
  (cond ((stringp proj)
	 (let ((l (assoc proj org-publish-project-alist)))
	   (if l
	       (cdr l)
	     (error "No project \"%s\" registered" proj))))
	((listp proj)
	 proj)
	(t
	 (error "Need a project name or plist"))))

(defun org-attach-publish---pub-dir (proj)
  "Return the publishing directory for attachments for project PROJ.

This will look first for a property ':attachments-publishing-directory'. If
not found, it will then look for ':attachments-project' and, if found, use
this as a key into 'org-publish-project-alist' to retrieve a project and
its publishing directory."
  (let ((info (org-attach-publish--project-name-or-plist proj)))
    (or (plist-get info :attachments-publishing-directory)
	(let ((pub-name (plist-get info :attachments-project)))
	  (if pub-name
	      ;; got a project name, look it up
	      (let ((pub-info (org-attach-publish--project-name-or-plist pub-name)))
		;; got a project, extract its publishing directory
		(plist-get pub-info :publishing-directory))

	    (error "No publishing directory or project provided"))))))

(defun org-attach-publish--base-dir (proj)
  "Return the base directory for attachments from a project PROJ.

This looks up the ':attachments-base-directory' property."
  (let ((proj (org-attach-publish--project-name-or-plist proj)))
    (or (plist-get info :attachments-base-directory)
	(error "No attachments base directory provided"))))

(defun org-attach-publish--file-within-project (proj fn)
  "Convert a file name F within PROJ to an absolute file name.

This looks up the project's ':base-directory' property."
  (let* ((info (org-attach-publish--project-name-or-plist proj))
	 (doc-dir (plist-get info :base-directory)))
    (concat doc-dir (f-path-separator) f)))

;; these next two feel wrong still...

(defun org-attach-publish--common-pub-dir (p1 p2)
  "Return the common root publication directory of two paths P1 and P2."
  (sd/f-common-prefix-path p1 p2))

(defun org-attach-publish--base-dir-rel (proj fn)
  "Return a relative link to the attachments dirsctory of FN that is part
of a project PROJ."
  (let* ((info (org-attach-publish--project-name-or-plist proj))
	 (fn (sd/f-split-path fn))
	 (attachments-dir (sd/f-split-path (org-attach-publish--base-dir info)))
	 (prefix (sd/f-common-prefix-path fn attachments-dir)))
    (sd/f-join-path (append (sd/f-to-prefix-path fn prefix)
			    (nthcdr (length prefix) attachments-dir)))))

;; to be re-done...


;; ---------- File name utilities ----------

(defun org-attach-publish--uniquify (fn dir)
  "Create a unique version of FN within DIR.

This adds a number suffix to the filename before any extension to disambiguate.
The intended use is that FN is a file within a project whose base directory
is DIR."
  (let* ((stem (f-no-ext fn))
	 (ext (f-ext fn))
	 (i 1)
	 (suffix "")
	 (ufn stem))
    (while (progn
	     (setq fn (format "%s%s%s%s.%s" dir (f-path-separator) stem suffix ext))
	     (f-exists-p fn))
      (setq suffix (format "-%d" i))
      (setq i (+ i 1)))
    (format "%s%s.%s" ufn suffix ext)))

(defun org-attach-publish-uniquify (proj fn)
  "Make FN unique within project PROJ.

FN is expressed relative to the base directory of PROJ."
  (let* ((info (org-attach-publish--project-name-or-plist proj))
	 (doc-dir (plist-get info :base-directory)))
    (org-attach-publish--uniquify fn doc-dir)))

(defun org-attach-publish-orgify (fn)
  "Ensure FN has a .org extension."
  (format "%s.org" (f-no-ext fn)))

(defun org-attach-publish-create-datetree-file-name (proj &optional slug)
  "Create a filename for a post in PROJ using the year/month/date tree form.

If SLUG is given, it is used as the basis for the filename. The filename
will be unique within the leaf of the tree."
  (let* ((info (org-attach-publish--project-name-or-plist proj))
	 (doc-dir (plist-get info :base-directory))
	 (today (decode-time))
	 (year (decoded-time-year today))
	 (month (decoded-time-month today))
	 (day (decoded-time-day today))
	 (fn (format "%d/%02d/%02d/%s.org"
		     year month day
		     (if slug
			 (if (equal slug "")
			     org-attach-publish-slug
			   slug)
		       org-attach-publish-slug))))
    (org-attach-publish--uniquify fn doc-dir)))


;; ---------- Creating notes with attachments ----------

(defun org-attach-publish-create-note (proj f)
  "Create a file F in project PROJ, returning a buffer.

F is expressed relative to the base directory for PROJ, and is creeated
if it doesn't already exist. The buffer is set to store any attachments
in the location specified in PROJ's ':attachments-base-directory' property."
  (interactive)
  (let* ((fn (org-attach-publish--file-within-project proj f)))
    (if (f-exists-p fn)
	;; file exists, visit it
	(let ((buf (get-file-buffer fn)))
	  (or buf
	      (let ((buf (create-file-buffer fn)))
		(set-buffer buf)
		buf)))

      ;; file doesn't exist, create and populate it
      (progn
	(f-mkdir-full-path (f-dirname fn))
	(let ((buf (create-file-buffer fn))
	      (rel (org-attach-publish--base-dir-rel proj fn)))
	  (set-buffer buf)
	  (set-visited-file-name fn t)  ; create-file-buffer doesn't set the visited file

	  ;; set the attachments directory for the capture
	  (make-local-variable 'org-attach-id-dir)
	  (setq org-attach-id-dir rel)

	  ;; add the header comment
	  (goto-char (point-min))
	  (insert (format "# -*- org-attach-id-dir: \"%s\"; -*-\n" rel))

	  buf)))))


(provide 'ox-attach-publish-frontend)

;;; ox-attach-publish-frontend.el ends here
