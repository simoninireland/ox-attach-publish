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

(require 'ox-attach-publish-f)

;; All to be re-written

(defun sd/org-attachments-pub-dir (info)
  "Return the publishing directory for attachments by parsing a project's INFO plist.

This will look first for a property ':attachments-publishing-directory'. If
not found, it will then look for ':attachments-project' and, if found, use
this as a key into 'org-publish-project-alist' to retrieve a project and
its publishing directory."
  (or (plist-get info :attachments-publishing-directory)
      (let ((projname (plist-get info :attachments-project)))
	(if projname
	    ;; got a project name, look it up
	    (let ((proj (assoc projname org-publish-project-alist)))
	      (if proj
		  ;; got a project, extract its publishing directory
		  (plist-get (cdr proj) :publishing-directory)
		(error "No project \"%s\" declared" projname)))
	  (error "No publishing directory or project provided")))))

(defun sd/org-attachments-base-dir (info)
  "Return the base directory for attachments from a project's INFO plist.

This looks up the ':attachments-base-directory' property."
  (or (plist-get info :attachments-base-directory)
      (error "No attachments base directory provided")))

(defun sd/org-attachments--common-pub-dir (p1 p2)
  "Return the common root publication directory of two paths P1 and P2."
  (sd/f-common-prefix-path p1 p2))


;; ---------- Extracting information from org parse trees ----------

(defun sd/org-attachments-element-id (e)
  "Return the id associated with element E by tracing up the parse tree.

The id is associated with the shallowest headline with the 'ID:' property."
  (or (org-element-property :ID e)
      (let ((p (org-element-property :parent e)))
	(if p
	    (sd/org-attachments-element-id p)
	    nil))))

(defun sd/org-attachments-element-id-dir (e)
  "Return the attachment directory associated with element E."
  (let ((id (sd/org-attachments-element-id e)))
    (if id
	(org-attach-dir-from-id id)
      (error "No ID property for attachments"))))


;; ---------- Relative link construction ----------

(defun sd/org-publish-attachments-rel-attachments-base-dir (fn info)
  "Return a relative link to the attachments dirsctory of FN that is part
of a project described by the INFO plist."
  (let* ((fn (sd/f-split-path fn))
	 (attachments-dir (sd/f-split-path (sd/org-attachments-base-dir info)))
	 (prefix (sd/f-common-prefix-path fn attachments-dir)))
    (sd/f-join-path (append (sd/f-to-prefix-path fn prefix)
			    (nthcdr (length prefix) attachments-dir)))))

(defun sd/org-attachments--relative-path-to-attachment (doc attach e info)
  "Return the relative path for a file: link from DOC to ATTACH from element E.

Constructing the path consists of three stages:

  - create the \"up\" path from DOC to the root of its publication directory
  - create the \"attach\" publication path from the root of its publication directory
  - remove the documents's publication directory from the \"attach\" path
    to create the \"down\" path
  - concatenate the \"up\" and \"down\" paths

The directory manipulations use paths extracted from the INFO plist of
the current publishing project."
  (let* ((doc-pub-path (sd/f-split-path (plist-get info :publishing-directory)))
	 (doc-fn-path (sd/f-split-path (plist-get info :output-file)))
	 (attach-base-path (sd/f-split-path (sd/org-attachments-base-dir info)))
	 (attach-pub-path (sd/f-split-path (sd/org-attachments-pub-dir info)))
	 (attach-rel (nthcdr (- (length attach-pub-path) 1) attach-base-path))
	 (pub-path (sd/org-attachments--common-pub-dir doc-pub-path attach-pub-path))
	 (attach-dir-path (sd/org-attachments-element-id-dir e))
	 (attach-fn-path (append attach-rel
				 (sd/f-split-path attach-dir-path)
				 (sd/f-split-path attach)))
	 (attach-fn-full-path (sd/f-merge-common-suffix-prefix attach-pub-path attach-fn-path))
	 (down (nthcdr (length pub-path) attach-fn-full-path))
	 (up (sd/f-to-prefix-path doc-fn-path doc-pub-path)))
    (sd/f-join-path (append up down))))


;; ---------- Parse tree filter ----------

(defun sd/org-attachments-filter-parse-tree (tree backend info)
  "Re-write attachment:-type links to file:-type links.

The necessary information for this re-writing is extracted from
the INFO plist, which contains all the properties included in the
publishing project.

To use this filter add a property ':filter-parse-tree' to the
publishing project pointing to this function, and ensure that the
necessary attachments properties are also provided."
  (org-element-map tree 'link
    #'(lambda (l)
	(when (equal (org-element-property :type l) "attachment")
	  ;; got an attachment link, re-write
	  (let* ((doc (plist-get info :output-file))
		 (attach (org-element-property :path l))
		 (rel (sd/org-attachments--relative-path-to-attachment doc attach l info)))
	    (plist-put (nth 1 l) :type "file")
	    (plist-put (nth 1 l) :path rel)))))
  tree)


(provide 'ox-attach-publish-machinery)

;;; ox-attach-publish-machinery.el ends here
