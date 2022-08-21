;;; ox-publish-attachmenmts-f.el -- File-handling utilities  -*- lexical-binding: t -*-
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

;; Some file-handling routines that turn file names into lists, because
;; everything in Lisp easier with lists...

;;; Code:

(require 'f)
(require 's)


;; ---------- List utilities ----------

;; These functions are implemented using recursion and so are only
;; safe for "short" lists. Since we're dealing with file paths
;; this is safe enough.

(defun ox-attach-publish--from-last (e l)
  "Return the suffix of L starting from the rightmost instance of E."
  (let ((suffix (member e l)))
    (if suffix
	(or (sd/from-last e (cdr suffix))
	    suffix)
      nil)))

(defun ox-attach-publish--prefix-p (p l)
  "Test whether a list P is a prefix of a list L."
  (cond ((null p)
	 t)
	((null l)
	 nil)
	((equal (car p) (car l))
	 (sd/prefix-p (cdr p) (cdr l)))
	(t
	 nil)))

(defun ox-attach-publish--dedouble (l)
  "Combine any repeated adjacent elements in L."
  (cond ((null l)
	 nil)
	((null (cdr l))
	 l)
	((equal (car l) (cadr l))
	 (cons (car l) (cdr (ox-attach-publish--dedouble (cdr l)))))
	(t
	 (cons (car l) (ox-attach-publish--dedouble (cdr l))))))


;; ---------- Filenames to file path lists ----------

(defun ox-attach-publish--split-path (fn)
  "Return filename FN as a list of elements.

The leading element will be \"\" for an absolute filename."
  (cond ((equal fn "")
	 nil)
	((equal fn "/")
	 "")
	(t
	 (s-split (f-path-separator) fn))))

(defun ox-attach-publish--join-path (p)
    "Join a file path P into a filename.

If the path begins with \"\" the filename will be absolute. Trailing,
repeated, and interior \"\" will all be ignored (and will not be created
for paths derived from `ox-attach-publish--split-path')."
    (cond ((null p)
	   nil)
	  ((equal p '(""))
	   "/")
	  (t
	   (let* ((f-single (ox-attach-publish--dedouble p))
		  (f-trailing (if (equal (car (last f-single)) "")
				  (butlast f-single)
				f-single)))
	     (s-join (f-path-separator) f-trailing)))))


;; ---------- Prefix management ----------

(defun ox-attach-publish--common-prefix (p1 p2)
  "Return the largest common prefix of two file paths."
  (cond ((or (null p1) (null p2))
	 nil)
	((equal (car p1) (car p2))
	 (cons (car p1) (ox-attach-publish--common-prefix (cdr p1) (cdr p2))))
	(t
	 nil)))

(defun ox-attach-publish--common-suffix (p1 p2)
  "Return the path that concatenates P1 and P2, merging any suffix of P1 that is a prefix of P2."
  (let* ((f2 (car p2))
	 (suffix1 (ox-attach-publish--from-last f2 p1)))
    (if (and suffix1
	     (ox-attach-publish--prefix-p suffix1 p2))
	(append p1 (nthcdr (length suffix1) p2))
      (append p1 p2))))

(defun ox-attach-publish--rel-prefix (p cp)
  "Return the path from the path P to the path CP.

CP should be a prefix of P. The relative path will consist of zero
or more \"..\" elements."
  (let ((ups (- (- (length p) (length cp)) 1)))
    (make-list ups "..")))


(provide 'ox-attach-publish-f)

;;; ox-attach-publish-f.el ends here
