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

(defun sd/from-last (e l)
  "Return the suffix of L starting from the rightmost instance of E."
  (let ((suffix (member e l)))
    (if suffix
	(or (sd/from-last e (cdr suffix))
	    suffix)
      nil)))

(defun sd/prefix-p (p l)
  "Test whether a list P is a prefix of a list L."
  (cond ((null p)
	 t)
	((null l)
	 nil)
	((equal (car p) (car l))
	 (sd/prefix-p (cdr p) (cdr l)))
	(t
	 nil)))


;; ---------- Unique file names ----------

(defun sd/f-uniq (stem ext)
  "If a file STEM.EXT already exists, add a unique suffix to STEM
to make a unique filename."
  (let ((i 1)
	(suffix "")
	fn)
    (while (progn
	     (setq fn (format "%s%s.%s" stem suffix ext))
	     (f-exists-p fn))
      (setq suffix (format "-%d" i))
      (setq i (+ i 1)))
    fn))


;; ---------- Filenames to file path lists ----------

(defun sd/f-split-path (fn)
  "Return filename FN as a list of elements.

The leading element will be \"\" for an absolute filename."
  (if (equal fn "")
      nil
    (s-split (f-path-separator) fn)))

(defun sd/f-join-path (p)
  "Join a file path P into a filename.

If the path begins with \"\" the filename will be absolute."
  (s-join (f-path-separator) p))


;; ---------- Prefix management ----------

(defun sd/f-common-prefix-path (p1 p2)
  "Return the largest common prefix of two file paths."
  (cond ((or (null p1) (null p2))
	 nil)
	((equal (car p1) (car p2))
	 (cons (car p1) (sd/f-common-prefix-path (cdr p1) (cdr p2))))
	(t
	 nil)))

(defun sd/f-merge-common-suffix-prefix (p1 p2)
  "Return the path that concatenates P1 and P2, merging any suffix of P1 that is a prefix of P2."
  (let* ((f2 (car p2))
	 (suffix1 (sd/from-last f2 p1)))
    (if (and suffix1
	     (sd/prefix-p suffix1 p2))
	(append p1 (nthcdr (length suffix1) p2))
      (append p1 p2))))

(defun sd/f-to-prefix-path (p cp)
  "Return the path from the path P to the path CP.

CP should be a prefix of P. The relative path will consist of zero
or more \"..\" elements."
  (let ((ups (- (- (length p) (length cp)) 1)))
    (make-list ups "..")))


(provide 'ox-attach-publish-f)

;;; ox-attach-publish-f.el ends here
