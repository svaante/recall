;;; process-history-consult.el --- Process history consult integration -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Daniel Pettersson
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.0.1
;; Homepage: https://github.com/svaante/process-history
;; Package-Requires: ((emacs "29.1") (consult))

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

;; To enable usage of consult backed completing-read interface, set
;; the default completing read function for process-history.

;; (setopt process-history-completing-read-fn
;;   'process-history-consult-completing-read)

;;; Code:

(require 'process-history)
(require 'consult)

(defun process-history-consult-completing-read (prompt &optional predicate)
  "Read a string in the minibuffer, with completion.
PROMPT is a string to prompt with; normally it ends in a colon and a
space.
PREDICATE is an optional function taking command string and
`process-history--item'.
Completes from collection based on `process-history'."
  (let* ((alist
          (seq-filter (or predicate 'identity)
                      (process-history--collection)))
         (annotate-fn-1
          (process-history--make-affixation alist))
         (annotate-fn
          ;; HACK Make consult--multi work with `affixation-function'
          ;;      we have to pop tofu stuff before calling affixiation
          ;;      like function then pop it back on.
          (lambda (cand)
            (pcase-let* ((string (cdr (get-text-property 0 'multi-category cand)))
                         (`(,candidate ,prefix ,suffix) (funcall annotate-fn-1 string)))
              (list (consult--tofu-append candidate (consult--tofu-get cand))
                    prefix suffix))))
         ;; HACK Let embark know that the real string needs to be
         ;;      extracted from text property 'multi-category.
         (embark-transformer-alist
          (when (boundp 'embark-transformer-alist)
            (append embark-transformer-alist
                    '((hack-multi-category . embark--refine-multi-category)))))
         (sources
          `((:name "Active"
                   :narrow ?a
                   :items ,(mapcan (pcase-lambda (`(,str . ,item))
                                     (unless (process-history--item-exit-code item)
                                       (list str)))
                                   alist))
            (:name "Exited"
                   :narrow ?e
                   :items ,(mapcan (pcase-lambda (`(,str . ,item))
                                     (when (process-history--item-exit-code item)
                                       (list str)))
                                   alist))
            (:name "Project"
                   :narrow ?p
                   :hidden t
                   :items
                   ,(when-let* ((root (consult--project-root))
                                (root (abbreviate-file-name root)))
                      (mapcan (pcase-lambda (`(,str . ,item))
                                (when (equal root (process-history--item-directory item))
                                  (list str)))
                              alist)))
            ,(let ((directory (abbreviate-file-name default-directory)))
               `(:name ,(format "Directory (%s)" directory)
                       :narrow ?d
                       :hidden t
                       :items
                       ,(mapcan (pcase-lambda (`(,str . ,item))
                                  (when (equal directory (process-history--item-directory item))
                                    (list str)))
                                alist)))))
         (match (car (consult--multi
                      (cl-loop for source in sources
                               collect (append source `(:category process-history)))
                      :prompt prompt
                      ;; HACK Same issue different package.
                      ;;      See `marginalia-annotate-multi-category'.
                      :category 'hack-multi-category
                      :annotate annotate-fn
                      :require-match t
                      :sort nil))))
    (alist-get match alist nil nil 'equal)))

(provide 'process-history-consult)
;;; process-history-consult.el ends here
