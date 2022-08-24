;;; ox-attach-publish-machinery.el -- Core attachment machinery  -*- lexical-binding: t -*-

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

;; This is the back-end machinery used for managing attachments and
;; publication.

;;; Code:

(require 'ox)
(require 'ox-attach-publish-f)


;; ---------- Project accdss ----------

(defun org-attach-publish--project-name-or-plist (proj)
  "Return the plist of PROJ.

PROJ may be a plist itself, a string corresponding to a project
in 'org-publish-project-alist', or a list consisting of a name
nd a plist such as you'd get by calling

   (assoc \"projname\" org-publish-project-alist)

to lookup a project by name."
  (cond ((stringp proj)
	 (let ((l (assoc proj org-publish-project-alist)))
	   (if l
	       (cdr l)
	     (error "No project \"%s\" registered" proj))))
	((listp proj)
	 (if (stringp (car proj))
	     (cdr proj)
	   proj))
	(t
	 (error "Need a project name or plist"))))

(defun org-attach-publish--attachments-pub-dir (proj)
  "Return the publishing directory for attachments for project PROJ.

This will first look for a property ':attachments-project~' specifying
the project that will publish the attachments. If ':attachments-base-directory'
is also defined this is interpreted as a relative path to be appended to
the project's puiblishing directory so that attachments are published
in their own sub-directory. This can help keep the URL namespaces clean.

If ':attachments-project' is not given then ':attachments-publishing-directory'
will be looked for again and this time treated as an ordinary path to
publish the attachments to. This then needs to be in the scope of some
published project in order for the files to be accessible."
  (let* ((info (org-attach-publish--project-name-or-plist proj))
	 (attachments-project (plist-get info :attachments-project))
	 (attachments-base-subdirectory (plist-get info :attachments-base-directory))
	 (attachments-publishing-directory (plist-get info :attachments-publishing-directory)))
    (if attachments-project
	(let* ((pub-info (org-attach-publish--project-name-or-plist attachments-project))
	       (attachments-publishing-directory (plist-get pub-info :publishing-directory)))
	  (if attachments-base-subdirectory
	      (concat attachments-publishing-directory
		      (f-path-separator)
		      attachments-base-subdirectory)
	    attachments-publishing-directory))
      (or attachments-publishing-subdirectory
	  (error "No publishing project or directory given for publishing attachments")))))

(defun org-attach-publish--attachments-base-dir (proj)
  "Return the base directory for attachments from a project PROJ.

This will first look for a property ':attachments-project~' specifying
the project that will publish the attachments. If ':attachments-base-directory'
is also defined this is interpreted as a relative path to be appended to
the project's base directory so that attachments are held
in their own sub-directory.

If ':attachments-project' is not given then ':attachments-base-directory'
will be looked for again and this time treated as an ordinary path to
store the attachments."
  (let* ((info (org-attach-publish--project-name-or-plist proj))
	 (attachments-project (plist-get info :attachments-project))
	 (attachments-base-subdirectory (plist-get info :attachments-base-directory)))
    (if attachments-project
	(let* ((pub-info (org-attach-publish--project-name-or-plist attachments-project))
	       (attachments-base-directory (plist-get pub-info :base-directory)))
	  (if attachments-base-subdirectory
	      (concat attachments-base-directory
		      (f-path-separator)
		      attachments-base-subdirectory)
	    attachments-base-directory))
      (or attachments-base-subdirectory
	  (error "No publishing project or directory given for storing attachments")))))

(defun org-attach-publish--file-within-project (proj fn)
  "Convert a file name F within PROJ to an absolute file name.

This looks up the project's ':base-directory' property."
  (let* ((info (org-attach-publish--project-name-or-plist proj))
	 (doc-dir (plist-get info :base-directory)))
    (concat doc-dir
	    (f-path-separator)
	    fn)))

;; these next two feel wrong still...

(defun org-attach-publish--common-pub-dir (p1 p2)
  "Return the common root publication directory of two paths P1 and P2."
  (org-attach-publish--common-prefix p1 p2))

(defun org-attach-publish--base-dir-rel (proj fn)
  "Return a relative link to the attachments dirsctory of FN that is part
of a project PROJ."
  (let* ((info (org-attach-publish--project-name-or-plist proj))
	 (fn (org-attach-publish--split-path fn))
	 (attachments-dir (org-attach-publish--split-path (org-attach-publish--base-dir info)))
	 (prefix (org-attach-publish--common-prefix fn attachments-dir)))
    (org-attach-publish--join-path (append (org-attach-publish--rel-prefix fn prefix)
					  (nthcdr (length prefix) attachments-dir)))))

;; to be re-done...


;; ---------- Extracting information from org parse trees ----------

(defun org-attach-publish--element-id (e)
  "Return the id associated with element E by tracing up the parse tree.

The id is associated with the shallowest headline with the 'ID:' property."
  (or (org-element-property :ID e)
      (let ((p (org-element-property :parent e)))
	(if p
	    (org-attach-publish--element-id p)
	    nil))))

(defun org-attach-publish--element-id-dir (e info)
  "Return the attachment directory associated with element E in project INFO.

The directory is returned as a path list relative to the project's attachment
publishing directory"
  (let ((id (org-attach-publish--element-id e)))
    (if id
	(let* ((org-attach-id-dir (org-attach-publish--attachments-pub-dir info))
	       (dir (org-attach-dir-from-id id)))
	  (org-attach-publish--remove-prefix (org-attach-publish--split-path org-attach-id-dir)
					     (org-attach-publish--split-path dir)))
      (error "No ID property for attachments"))))


;; ---------- Parse tree filter ----------

(defun org-attach-publish--filter-parse-tree (tree backend info)
  "Re-write attachment:-type links to file:-type links.

The necessary information for this re-writing is extracted from
the INFO project plist, which contains all the properties included in the
publishing project."
  (org-element-map tree 'link
    #'(lambda (l)
	(when (equal (org-element-property :type l) "attachment")
	  ;; got an attachment link, re-write
	  (let* ((doc (org-attach-publish--split-path (plist-get info :output-file)))
		 (doc-publishing-dir (org-attach-publish--split-path (plist-get info :publishing-directory)))
		 (attach (org-attach-publish--split-path (org-element-property :path l)))
		 (attach-publishing-dir (org-attach-publish--split-path (org-attach-publish--attachments-pub-dir info)))
		 (prefix (org-attach-publish--common-prefix doc-publishing-dir
							    attach-publishing-dir))
		 (attach-subdir (org-attach-publish--element-id-dir l info))
		 (up (make-list (- (length (org-attach-publish--remove-prefix prefix doc)) 1) ".."))
		 (down (org-attach-publish--remove-prefix prefix attach-publishing-dir))
		 (rel (org-attach-publish--join-path (append up
							     down
							     attach-subdir
							     attach))))
	    (princ up)
	    (org-element-put-property l :type "file")
	    (org-element-put-property l :path rel)))))
  tree)


(provide 'ox-attach-publish-machinery)
;;; ox-attach-publish-machinery.el ends here
