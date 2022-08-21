;;; test-f.el --- File path tests -*- lexical-binding: t -*-

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

;; Test the file paths functions that manipulate filenames using
;; lists -- 'cos everything in Lisp makes more sense as a list

;;; Code:

(require 'ert)


;; ---------- List utilities ----------

(ert-deftest from-last ()
  ;; first element
  (should (equal (ox-attach-publish--from-last 1 '(1 2 3))
		 '(1 2 3)))

  ;; secont element
  (should (equal (ox-attach-publish--from-last 2 '(1 2 3))
		 '(2 3)))

  ;; duplicate value
  (should (equal (ox-attach-publish--from-last 2 '(1 2 3 4 2 7 8))
		 '(2 7 8)))

  ;; last element
  (should (equal (ox-attach-publish--from-last 2 '(1 2))
		 '(2)))

  ;; not present
  (should (null (ox-attach-publish--from-last 4 '(1 2 3))))

  ;; empty list
  (should (null (ox-attach-publish--from-last 1 nil)))

  ;; lists of lists
  (should (equal (ox-attach-publish--from-last '(1 2) '(1 2 (1 2) 3))
		 '((1 2) 3))))

(ert-deftest prefix-p ()
  ;; ordinary prefix pass
  (should (ox-attach-publish--prefix-p '(1 2) '(1 2 3)))

  ;; ordinary prefix fail
  (should-not (ox-attach-publish--prefix-p '(1 2) '(2 3 4)))

  ;; empty prefix (defined as true for any list)
  (should (ox-attach-publish--prefix-p nil '(1 2 3)))

  ;; prefix too long
  (should-not (ox-attach-publish--prefix-p '(1 2 3) '(1 2)))

  ;; empty list
  (should-not (ox-attach-publish--prefix-p '(1 2 3) nil))

  ;; lists of lists
  (should (ox-attach-publish--prefix-p '(1 2 (1 2) 3) '(1 2 (1 2) 3 4 5))))


;; ---------- Path splitting and joining ----------

(ert-deftest split-path ()
  ;; absolute path
  (should (equal (car (ox-attach-publish--split-path "/home/user"))
		 ""))

  ;; relative path
  (should-not (equal (car (ox-attach-publish--split-path "home/user"))
		     ""))

  ;; correct elements
  (should (equal (ox-attach-publish--split-path "/home/user/dir/file")
		 '("" "home" "user" "dir" "file")))

  ;; no trailing slashes
  (should-not (equal (last (ox-attach-publish--split-path "/home/user/"))
		     ""))
  (should (equal (ox-attach-publish--split-path "/home/user/")
		 '("" "home" "user")))

  ;; root
  (should (equal (ox-attach-publish--split-path "/")
		 ""))

  ;; empty path
  (should-not (ox-attach-publish--split-path "")))

(ert-deftest dedouble ()
  ;; only adjacents
  (should (equal (ox-attach-publish--dedouble '(1 2 3 3 4 5 3))
		 '(1 2 3 4 5 3)))

  ;; triples are colapsed too
  (should (equal (ox-attach-publish--dedouble '(1 2 3 3 3 4))
		 '(1 2 3 4)))

  ;; trailing doubles
  (should (equal (ox-attach-publish--dedouble '(1 2 3 4 5 5))
		 '(1 2 3 4 5)))

  ;; trailing triples
  (should (equal (ox-attach-publish--dedouble '(1 2 3 3 3 ))
		 '(1 2 3)))

  ;; trailing and interior doubles
  (should (equal (ox-attach-publish--dedouble '(1 2 2 3 4 5 5))
		 '(1 2 3 4 5)))

  ;; no doubles
  (should (equal (ox-attach-publish--dedouble '(1 2 3 4 5))
		 '(1 2 3 4 5)))

  ;; all the same
  (should (equal (ox-attach-publish--dedouble '(1 1 1))
		 '(1)))

  ;; singleton
  (should (equal (ox-attach-publish--dedouble '(1))
		 '(1)))

  ;; empty list
  (should-not (ox-attach-publish--dedouble nil)))

(ert-deftest join-path ()
  ;; absolute path
  (should (equal (ox-attach-publish--join-path '("" "home" "user"))
		 "/home/user"))

  ;; relative path
  (should (equal (ox-attach-publish--join-path '("home" "user"))
		 "home/user"))

  ;; correct elements
  (should (equal (ox-attach-publish--join-path '("" "home" "user" "dir" "file"))
		 "/home/user/dir/file"))

  ;; no trailing slashes (even though these shouldn't be created)
  (should (equal (ox-attach-publish--join-path '("" "home" "user" ""))
		 "/home/user"))

  ;; no interior slashes (even though these shouldn't be created)
  (should (equal (ox-attach-publish--join-path '("" "home" "" "user"))
		 "/home/user"))

  ;; combine double slashes (even though these shouldn't be created)
  (should (equal (ox-attach-publish--join-path '("" ""  "home" "user"))
		 "/home/user"))

  ;; root
  (should (equal (ox-attach-publish--join-path '(""))
		 "/"))

  ;; empty path
  (should-not (ox-attach-publish--join-path nil)))


;; ---------- Prefix management ----------

(ert-deftest common-prefix-simple ()
  (should (equal (ox-attach-publish--common-prefix (ox-attach-publish--split-path "/home/user/one")
						   (ox-attach-publish--split-path "/home/user/two"))
		 (ox-attach-publish--split-path "/home/user"))))

(ert-deftest common-prefix-one-longer ()
  (should (equal (ox-attach-publish--common-prefix (ox-attach-publish--split-path "/home/user/one/two")
						   (ox-attach-publish--split-path "/home/user/two"))
		 (ox-attach-publish--split-path "/home/user"))))

(ert-deftest common-prefix-root ()
  (should (equal (ox-attach-publish--common-prefix (ox-attach-publish--split-path "/home/user/one/")
						   (ox-attach-publish--split-path "/user/two"))
		 (ox-attach-publish--split-path "/"))))

(ert-deftest common-prefix-none ()
  (should-not (ox-attach-publish--common-prefix (ox-attach-publish--split-path "home/user/one/")
						(ox-attach-publish--split-path "user/two"))))

(ert-deftest common-suffix-simple ()
  (should (equal (ox-attach-publish--common-suffix (ox-attach-publish--split-path "/home/user")
						   (ox-attach-publish--split-path "user/two"))
		 (ox-attach-publish--split-path "/home/user/two"))))

(ert-deftest common-suffix-none ()
  (should (equal (ox-attach-publish--common-suffix (ox-attach-publish--split-path "/home/user")
						   (ox-attach-publish--split-path "two"))
		 (ox-attach-publish--split-path "/home/user/two"))))
