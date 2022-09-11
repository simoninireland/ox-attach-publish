;;; ox-publish-attachmenmts-f.el -- File-handling utilities -*- lexical-binding: t -*-

;; Copyright (c) 2022 Free Software Foundation, Inc.

;; Author: Simon Dobson <simoninireland@gmail.com>
;; Maintainer: Simon Dobson <simoninireland@gmail.com>
;; Keywords: org, hypermedia, attachments
;; Homepage: https://github.com/simoninireland/ox-attach-publish
;; Package-Requires: ((emacs "27.2")(org "8.0"))

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

;; Some file-handling routines that turn file names into lists, because
;; everything in Lisp easier with lists...

;;; Code:

(require 'f)
(require 's)


;; ---------- List utilities ----------

;; These functions are implemented using recursion and so are only
;; safe for "short" lists. Since we're dealing with file paths
;; this is safe enough.

(defun org-attach-publish--from-last (e l)
  "Return the suffix of L starting from the rightmost instance of E."
  (let ((suffix (member e l)))
    (if suffix
	(or (org-attach-publish--from-last e (cdr suffix))
	    suffix)
      nil)))

(defun org-attach-publish--prefix-p (p l)
  "Test whether a list P is a prefix of a list L."
  (cond ((null p)
	 t)
	((null l)
	 nil)
	((equal (car p) (car l))
	 (org-attach-publish--prefix-p (cdr p) (cdr l)))
	(t
	 nil)))

(defun org-attach-publish--dedouble (l)
  "Combine any repeated adjacent elements in L."
  (cond ((null l)
	 nil)
	((null (cdr l))
	 l)
	((equal (car l) (cadr l))
	 (cons (car l) (cdr (org-attach-publish--dedouble (cdr l)))))
	(t
	 (cons (car l) (org-attach-publish--dedouble (cdr l))))))


;; ---------- Filenames to file path lists ----------

(defun org-attach-publish--deslash (p)
  "Remove any \"\" from file path P."
  (cond ((null p)
	 nil)
	((equal (car p) "")
	 (org-attach-publish--deslash (cdr p)))
	(t
	 (cons (car p) (org-attach-publish--deslash (cdr p))))))

(defun org-attach-publish--inner (p)
  "Remove any inner \"\" from file path P."
  (if (null p)
      nil
    (cons (car p) (org-attach-publish--deslash (cdr p)))))

(defun org-attach-publish--outer (p)
  "Ensure we have at most one leading \"\" on path P."
  (cond ((null p)
	 nil)
	((equal (car p) "")
	 (if (equal (cadr p) "")
	     (org-attach-publish--outer (cdr p))
	   p))
	(t
	 p)))

(defun org-attach-publish--detrailing (p)
  "Remove trailing \"\"s from P."
  (cond ((null p)
	 nil)
	((equal (car (last p)) "")
	 (org-attach-publish--detrailing (butlast p)))
	(t
	 p)))

(defun org-attach-publish--normalise-path (p)
  "Normalise the path P to remove any anomalies caused in construction.

This removes repeated leading slashes, inner slashes, and trailing slashes."
  (org-attach-publish--detrailing
   (org-attach-publish--inner
    (org-attach-publish--outer
     p))))

(defun org-attach-publish--split-path (fn)
  "Return filename FN as a list of elements.

The leading element will be \"\" for an absolute filename."
  (cond ((equal fn "")
	 nil)
	((equal fn "/")
	 '(""))
	(t
	 (org-attach-publish--normalise-path (s-split (f-path-separator) fn)))))

(defun org-attach-publish--join-path (p)
    "Join a file path P into a filename.

If the path begins with \"\" the filename will be absolute. Trailing,
repeated, and interior \"\" will all be ignored (and will not be created
for paths derived from `org-attach-publish--split-path')."
    (cond ((null p)
	   nil)
	  ((equal p '(""))
	   "/")
	  (t
	   (s-join (f-path-separator)
		   (org-attach-publish--normalise-path p)))))


;; ---------- Prefix management ----------

(defun org-attach-publish--common-prefix (p1 p2)
  "Return the largest common prefix of P1 and P2."
  (cond ((or (null p1) (null p2))
	 nil)
	((equal (car p1) (car p2))
	 (cons (car p1) (org-attach-publish--common-prefix (cdr p1) (cdr p2))))
	(t
	 nil)))

(defun org-attach-publish--remove-prefix (p1 p2)
  "Remove P1 if it is a preefix of P2. otherwise P2 is returned unchanged.

This can be used to convert an absolute filename within a project into
a relative filename, which makes relative link construction easier."
  (if (org-attach-publish--prefix-p p1 p2)
      (nthcdr (length p1) p2)
    p2))

(defun org-attach-publish--common-suffix (p1 p2)
  "Return the path that concatenates P1 and P2, merging any suffix of P1 that is a prefix of P2."
  (let* ((f2 (car p2))
	 (suffix1 (org-attach-publish--from-last f2 p1)))
    (if (and suffix1
	     (org-attach-publish--prefix-p suffix1 p2))
	(append p1 (nthcdr (length suffix1) p2))
      (append p1 p2))))


(provide 'ox-attach-publish-f)
;;; ox-attach-publish-f.el ends here
