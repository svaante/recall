;;; process-history-embark.el --- Process history embark integration -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Daniel Pettersson
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.0.1
;; Homepage: https://github.com/svaante/process-history
;; Package-Requires: ((emacs "29.1") (embark))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To enable embark bindings for `process-history' commands which use
;; an `completing-read' interface.

;; Require this file as (require 'process-history-embark)

;;; Code:

(require 'process-history)
(require 'embark)

(defvar-keymap embark-process-history-actions-map
  :doc "Process history actions"
  :parent embark-general-map
  "k" #'process-history-process-kill
  "r" #'process-history-rerun-with-async-shell-command
  "c" #'process-history-rerun-with-compile
  "!" #'process-history-rerun-with-async-shell-command
  "w" #'process-history-copy-as-kill-command
  "b" #'process-history-display-buffer
  "d" #'process-history-delete-item)

(add-to-list 'embark-keymap-alist
             '(process-history . embark-process-history-actions-map))

(defun process-history-export (candidates)
  (let ((alist (process-history--collection)))
    (process-history-list (mapcar (lambda (cand)
                                    (cdr (assoc cand alist)))
                                  candidates))))

(add-to-list 'embark-exporters-alist
             '(process-history . process-history-export))

(provide 'process-history-embark)
;;; process-history-embark.el ends here
