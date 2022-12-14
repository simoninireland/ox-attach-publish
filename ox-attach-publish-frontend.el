;;; ox-attach-publish-frontend.el --- Attachment publishing frontend -*- lexical-binding: t -*-

;; Copyright (c) 2022 Free Software Foundation, Inc.

;; Author: Simon Dobson <simoninireland@gmail.com>
;; Maintainer: Simon Dobson <simoninireland@gmail.com>
;; Keywords: Hypermedia, multimedia
;; Homepage: https://github.com/simoninireland/ox-attach-publish
;; Package-Requires: ((emacs "27.2") (org "8.0"))

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
;; All the functions in this file are intended for use in user code
;; (and no functions in other files need be referenced).

;;; Code:

(require 'f)
(require 'ox-html)
(require 'ox-attach-publish-f)


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
	(let* ((buf (create-file-buffer fn))
	       (info (org-attach-publish--project-name-or-plist proj))
	       (doc-dir (org-attach-publish--split-path (f-dirname fn)))
	       (attach-dir (org-attach-publish--split-path (org-attach-publish--attachments-base-dir info)))
	       (prefix (org-attach-publish--common-prefix doc-dir
							  attach-dir))
	       (up (make-list (length (org-attach-publish--remove-prefix prefix doc-dir)) ".."))
	       (down (org-attach-publish--remove-prefix prefix attach-dir))
	       (rel (org-attach-publish--join-path (append up
							   down))))
	  (set-buffer buf)
	  (set-visited-file-name fn t)  ; create-file-buffer doesn't set the visited file name

	  ;; set the attachments directory for the capture
	  (make-local-variable 'org-attach-id-dir)
	  (setq org-attach-id-dir rel)

	  ;; add the header comment
	  (goto-char (point-min))
	  (insert (format "# -*- org-attach-id-dir: \"%s\"; -*-\n" rel))

	  buf)))))


;; ---------- Publishing backend and function ----------

;; Derive an HTML backend that installs the attachment: link filter
(org-export-define-derived-backend 'html-with-attachments 'html
  :filters-alist '((:filter-parse-tree org-attach-publish--filter-parse-tree)))

(defun org-attach-publish-to-html (info fn pub-dir)
  "Publish FN as HTML with attachments to directory PUB-DIR using settings from the INFO plist.

Returns the published file name."
  (let* ((old-hooks org-export-before-parsing-hook)
	 (org-export-before-parsing-hook old-hooks))

    ;; remove the default link-expansion function from the hook
    (remove-hook 'org-export-before-parsing-hook #'org-attach-expand-links)

    ;; publish using the new backend, which will pick up the restricted
    ;; hook function using dynamic binding
    (org-publish-org-to 'html-with-attachments
			fn
			(concat (when (> (length org-html-extension) 0) ".")
				(or (plist-get info :html-extension)
				    org-html-extension
				    "html"))
			info pub-dir)))


(provide 'ox-attach-publish-frontend)
;;; ox-attach-publish-frontend.el ends here
