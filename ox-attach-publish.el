;;; ox-attach-publish.el -- Publish files attached to org documents -*- lexical-binding: t -*-

;; Copyright (c) 2022 Simon Dobson <simoninireland@gmail.com>

;; Author: Simon Dobson <simoninireland@gmail.com>
;; Maintainer: Simon Dobson <simoninireland@gmail.com>
;; Version: 0.1.1
;; Keywords: hypermedia, attachments
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

;; This code integrates files attachments to org-mode documents with the
;; org-mode publishing framework. This simplifies managing non-org content
;; alongside a set of org documents destined for a web site. Essentially
;; it lets documents intended for publicaiton have attached files such as
;; images, which they then manipulate or link to using attachment: links
;; in the normal way. When the documents are published to HTML the attachments
;; are published too, and the attachment: links are re-written to file: links
;; with the correct relative path.

;; ox-publish-attachments does the following:
;;
;; - Adds properties to publishing projects to specify where
;;   attached files should be stored
;; - Adds functions to set buffer-local properties on org files
;;   so that the attchments are managed properly
;; - Provides a filter to be added to a project to process
;;   attachment:-type links

;;; Code:

(require 'ox-attach-publish-machinery)
(require 'ox-attach-publish-frontend)

(provide 'ox-attach-publish)
;;; ox-attach-publish.el ends here
