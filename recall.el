;;; recall.el --- Recall Emacs subprocess -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Daniel Pettersson
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.0.1
;; Homepage: https://github.com/svaante/recall
;; Package-Requires: ((emacs "29.1"))

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

;; This package `advice-add's `make-process' and friends to store
;; metadata like; working directory, stdout, start time, end time,
;; exit code and version control revision for Emacs subprocesses.

;; It defines commands to view and manage both currently running
;; processes and historical processes, accessible via the minibuffer
;; and a specialized `tabulated-list' buffer.

;; Joining the functionality of bash reverse-i-search with `proced'.

;; Enable process surveillance with `recall-mode'.

;; Includes integration with `embark' and `consult'.

;; Note:
;; As this package advices core functionality, usage might have
;; unintended consequences.  Disable `recall-mode' at the
;; first signs of process spawning troubles.

;; Package is inspired by detached.el

;;; Code:

(require 'cl-macs)
(require 'tabulated-list)
(require 'tramp)
(require 'dired-aux)
(require 'esh-mode)


;;; Custom
(defgroup recall nil
  "Recall Emacs subprocesses."
  :prefix "recall-"
  :group 'applications)

(defcustom recall-directory "/tmp/"
  "Where to place logs."
  :type 'directory)

(defcustom recall-save-file (locate-user-emacs-file "recall")
  "File to store history."
  :type 'file)

(defcustom recall-buffer-match `((major-mode . compilation-mode)
                                 (major-mode . shell-command-mode)
                                 (major-mode . eshell-mode)
                                 ;; `dired-mode'
                                 ,(regexp-quote shell-command-buffer-name-async))
  "Record processes spawned in matching buffers.
See `buffer-match-p'."
  :type '(repeat (choice :tag "Condition"
			 regexp
			 (function :tag "Matcher function"))))

(defcustom recall-rerun-alist '((nil . async-shell-command)
                                ((major-mode . compilation-mode) . compile))
  "Rerun item from ALIST specification (CONDITION . FN).
Where condition is either an item in `recall-buffer-match',
`recall-this-command' or nil for anything else."
  :type 'alist)

(defcustom recall-prune-after (* 60 60 24 7 2) ;; two weeks
  "Prune history after seconds on `recall-save'."
  :type '(choice (natnum :tag "Prune history item after seconds.")
                 (const :tag "Never prune." nil)))

(defcustom recall-prune-keep-unique t
  "Keep unique item strings in history."
  :type 'boolean)

(defcustom recall-log-format "%Y-%02m-%02d_%02H_%02M:%02S_%N.log"
  "Log file format.
Warning: `recall' won't find old logs if changed.
See `time-stamp-format'."
  :type 'command)

(defcustom recall-format-alist
  '(("RC"        . (lambda (item)
                     (when-let ((code (recall--item-exit-code item)))
                       (propertize
                        (format "%s" code) 'face
                        (cond
                         ((and-let* ((process (recall--item-process item)))
                            (process-live-p process))
                          'default)
                         ((equal (recall--item-exit-code item) 0)
                          'recall-success-face)
                         (t 'recall-error-face))))))
    ("Start"     . (lambda (item)
                     (propertize (recall--format-time (recall--item-start-time item))
                                 'face 'recall-time-face)))
    ("Time"      . (lambda (item)
                     (propertize
                      (recall--relative-time
                       (- (time-to-seconds (recall--item-end-time item))
                          (time-to-seconds (recall--item-start-time item))))
                      'face 'recall-time-face)))
    ("Directory" . (lambda (item)
                     (propertize
                      (directory-file-name (recall--item-directory item))
                      'face 'recall-directory-face)))
    ("VC"        . (lambda (item)
                     (when-let ((vc (recall--item-vc item)))
                       (propertize vc 'face 'recall-vc-face))))
    ("Buffer"    . (lambda (item)
                     (let ((process (recall--item-process item)))
                       (when (process-live-p process)
                         (format "%s" (process-buffer process))))))
    ("PID"       . (lambda (item)
                     (ignore-errors
                       (format "%s" (process-id (recall--item-process item)))))))
  "Log item format alist.
Alist of (NAME . FN) pairs.  Where FN takes `recall--item' should
return string."
  :type 'alist)

(defcustom recall-list-format [("Item" 70 t)
                               ("Directory" 28 t :right-align t)
                               ("Time" 6 t :right-align t)
                               ("RC" 3 t :right-align t)
                               ("Start" 12 t :right-align t)
                               ("VC" 8 t)
                               ("Buffer" 25 t)
                               ("PID" 8 t)]
  "See `tabulated-list-format'.
Each NAME needs to exist in `recall-format-alist' to be
displayed correctly."
  :type '(vector (repeat :inline t sexp)))

(defcustom recall-completing-read-fn #'recall-completing-read
  "Function used to complete `item candidates.
See `recall-completing-read'."
  :type 'function)


;;; Faces
(defface recall-directory-face
  '((t :inherit dired-directory))
  "Face used in Directory column.")

(defface recall-time-face
  '((t :inherit font-lock-doc-face))
  "Face used to highlight time.")

(defface recall-success-face
  '((t :inherit success))
  "Face used in Code column for success status.")

(defface recall-error-face
  '((t :inherit error))
  "Face used in Code column for error status.")

(defface recall-vc-face
  '((t :inherit shadow))
  "Face used in VC column.")

(defface recall-condition-face
  '((t :inherit italic))
  "Face used in Condition column.")

(defface recall-log-overlay-face
  '((t :inherit font-lock-comment-face))
  "Face used in `recall-log-mode' info overlay.")


;;; Global vars
(defvar recall-items nil
  "List of `recall--item'.")


;;; Data
(cl-defstruct (recall--item (:type list))
  command directory exit-code start-time end-time vc condition process)


;;; Utils
(defun recall--item-to-string (item)
  ;; HACK Prettier paths in `eshell-mode'
  (if (eq this-command 'eshell-send-input)
      (buffer-substring-no-properties eshell-last-input-start
                                      (1- eshell-last-input-end))
    (string-trim
     (string-join (if (equal (nth 1 item) shell-command-switch)
                      (nthcdr 2 item)
                    item)
                  " "))))

(defun recall--log-file (item)
  (thread-last (recall--item-start-time item)
               (format-time-string recall-log-format)
               (file-name-concat recall-directory)))

(defun recall--relative-time (diff)
  (cond ((> diff (* 24 60 60)) (format-seconds "%dd %hh" diff))
        ((> diff (* 60 60)) (format-seconds "%hh %mm" diff))
        (t (format-seconds "%mm %ss%z" diff))))

(defun recall--format-time (time)
  (let ((diff (- (time-to-seconds nil)
                 (time-to-seconds time))))
    (if (> diff (* 7 24 60 60))
        (format-time-string "%b %d %H:%M" time)
      (format "%s ago" (recall--relative-time diff)))))

(defun recall--prune ()
  (setq recall-items
        (cl-loop with command-set = (make-hash-table :test 'equal)
                 for item in recall-items
                 for command = (recall--item-command item)
                 ;; An history item is unique if no other item
                 ;; shares the same item string.
                 for key = command
                 for unique-command-p = (not (gethash key command-set))
                 do (puthash key t command-set)
                 if (or (not recall-prune-after)
                        (< (- (time-to-seconds)
                              (time-to-seconds
                               (recall--item-start-time item)))
                           recall-prune-after)
                        (and recall-prune-keep-unique
                             unique-command-p))
                 collect item
                 else do (delete-file (recall--log-file item)))))

(defmacro recall--def-do-command (name base-command doc)
  (declare (indent 2))
  `(defun ,name ()
     ,doc
     (interactive nil recall-list-mode recall-log-mode)
     (let ((item
            (pcase major-mode
              ('recall-list-mode (tabulated-list-get-id))
              ('recall-log-mode recall--item)
              (_ (user-error "Expects `recall-list-mode' or `recall-log-mode'")))))
       (when (eq major-mode 'recall-list-mode)
         (forward-line))
       (,base-command item))))


;;; Latch on `make-process'
(defvar recall--parent-condition nil)

(defun recall--make-process (make-process &rest args)
  (let (command directory buffer condition)
    (cond
     ((and
       ;; Skip if not interactive
       this-command
       ;; Handle tramp process
       (or (not (equal signal-hook-function 'tramp-signal-hook-function))
           (plist-get args :file-handler))
       (setq command (recall--item-to-string (plist-get args :command))
             directory (or (plist-get args :directory) default-directory)
             buffer (let ((buffer (plist-get args :buffer)))
                      (pcase buffer
                        ((pred bufferp) buffer)
                        ((pred stringp) (get-buffer buffer))
                        (_ (current-buffer)))))
       ;; Skip empty items
       (not (string-empty-p command))
       ;; Check condition
       (setq condition
             ;; Using stack scoped `recall--parent-condition' to inherit
             ;; condition from rerun command.
             (or recall--parent-condition
                 (cl-find-if (lambda (condition)
                               (and (bufferp buffer)
                                    (buffer-match-p condition buffer)))
                             recall-buffer-match))))
      (let ((item
             (make-recall--item
              :command command
              :start-time (current-time)
              :directory (abbreviate-file-name directory)
              :condition condition
              ;; FIXME Should be possible to support other vc backends
              ;; TODO Would be nice if we could store dirty, clean etc.
              :vc (vc-working-revision (file-name-as-directory directory) 'Git))))
        (dired-create-empty-file (recall--log-file item))
        (plist-put args :filter
                   (recall--make-filter item (plist-get args :filter)))
        (plist-put args :sentinel
                   (recall--make-sentinel item (plist-get args :sentinel)))
        (push item recall-items)
        (setf (recall--item-process item) (apply make-process args))))
     (t
      (apply make-process args)))))

(defun recall--set-process-filter (set-filter process filter)
  (if-let ((item (car (cl-member process
                                 recall-items
                                 :key 'recall--item-process))))
      (funcall set-filter process (recall--make-filter item filter))
    (funcall set-filter process filter)))

(defun recall--set-process-sentinel (set-sentinel process sentinel)
  (if-let ((item (car (cl-member process
                                 recall-items
                                 :key 'recall--item-process))))
      (funcall set-sentinel process
               (recall--make-sentinel item sentinel))
    (funcall set-sentinel process sentinel)))

(defun recall--make-filter (item filter)
  (let ((log-file (recall--log-file item))
        (filter (or filter
                    (lambda (proc command)
                      (with-current-buffer (process-buffer proc)
                        (save-excursion
                          (goto-char (point-max))
                          (let ((inhibit-read-only t))
                            (insert command))))))))
    (lambda (proc command)
      (unwind-protect
          (funcall filter proc command)
        (let ((coding-system-for-write 'raw-text))
          (write-region command nil log-file 'append 'no-echo))))))

(defun recall--make-sentinel (item sentinel)
  (let ((sentinel (or sentinel 'ignore)))
    (lambda (proc msg)
      (unwind-protect
          (funcall sentinel proc msg)
        (when (memq (process-status proc) '(exit signal))
          (setf (recall--item-end-time item)
                (current-time)
                (recall--item-exit-code item)
                (process-exit-status proc)))))))


;;; List
(defvar-local recall-list-items nil)

(defun recall--list-refresh ()
  (cl-loop for (name . item) in (recall--collection recall-list-items)
           for desc =
           (cl-loop for (col) across tabulated-list-format collect
                    (cond ((equal col "Item") name)
                          ((funcall (cdr (assoc col recall-format-alist)) item))
                          (t "--"))
                    into desc finally return (apply 'vector desc))
           collect (list item desc) into entries
           finally do
           (setq tabulated-list-entries entries)))

(defvar-keymap recall-list-mode-map
  :doc "Local keymap for `recall-list-mode' buffers."
  :parent tabulated-list-mode-map
  "C-m"           #'recall-do-find-log
  "D"             #'recall-do-process-kill
  "r"             #'recall-do-rerun
  "e"             #'recall-do-rerun-edit
  "o"             #'recall-do-buffer
  "w"             #'recall-do-copy-as-kill-command
  "d"             #'recall-do-delete
  "<mouse-2>"     #'recall-do-find-log
  "<follow-link>" 'mouse-face)

(defvar recall-list-mode nil)

(define-derived-mode recall-list-mode tabulated-list-mode "Recall Command List"
  "List of live and exited Emacs subprocesses."
  :interactive nil
  (setq-local buffer-stale-function (lambda (&optional _noconfirm) 'fast)
              recall-list-mode t)
  (setq tabulated-list-use-header-line t
        tabulated-list-format recall-list-format)
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'recall--list-refresh nil t))

;;;###autoload
(defun recall-list (&optional items)
  "Display an list of live and exited Emacs subprocesses.
If ITEMS is non nil display all processes."
  (interactive)
  (let ((buffer (get-buffer-create "*recall*")))
    (with-current-buffer buffer
      (recall-list-mode)
      (setq recall-list-items items)
      (revert-buffer)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))


;;; Logs
(defvar recall--log-filter-functions nil)

(defvar-local recall--item nil)

(defvar-keymap recall-log-mode-map
  :doc "Local keymap for `recall-mode' buffers."
  "r" #'recall-do-rerun
  "w" #'recall-do-copy-as-kill-command
  "d" #'recall-do-delete)

(define-derived-mode recall-log-mode special-mode "Log"
  "Mode active in `recall' log files."
  :interactive nil
  ;; TODO Auto revert overlay info
  (setq recall--item
        (cl-find-if (lambda (item)
                      (equal (recall--log-file item) buffer-file-name))
                    recall-items))
  (unless recall--item
    (user-error "Unable find connection with log file %s in `recall-items'"
                buffer-file-name))
  (unless (file-exists-p buffer-file-name)
    (let ((inhibit-read-only t))
      (save-excursion
        (insert (propertize "* Log file has been deleted *"
                            'face 'warning)))))
  (let ((before-string
         (cl-loop with max-length =
                  (apply 'max (mapcar (lambda (x) (length (car x)))
                                      recall-format-alist))
                  for (name . accessor) in
                  (cons '("Command" . recall--item-command) recall-format-alist)
                  for value = (funcall accessor recall--item)
                  when value concat (format (format "%%%ds: %%s\n" max-length)
                                            name value)
                  into before-string
                  finally return
                  (concat (propertize before-string
                                      'face 'recall-log-overlay-face)
                          "\n")))
         (overlay
          (or (cl-find 'recall-log-overlay
                       (overlays-in (point-min) (point-max))
                       :key (lambda (ov) (overlay-get ov 'category)))
              (make-overlay (point-min) (point-min)))))
        (overlay-put overlay 'category 'recall-log-overlay)
        (overlay-put overlay 'before-string before-string))
  (setq buffer-file-name nil
        default-directory (recall--item-directory recall--item)
        mode-line-buffer-identification
        (append mode-line-buffer-identification
                (list (format " {%s}" (recall--item-command recall--item)))))
  (let ((inhibit-read-only t))
    (run-hooks 'recall--log-filter-functions)))

(defun recall--log-asni-color-filter ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-hook 'recall--log-filter-functions #'recall--log-asni-color-filter)


;;; Complete
(defun recall--collection (&optional items)
  (let ((string-count (make-hash-table :test 'equal))
        (string-unique-p (make-hash-table :test 'equal)))
    (cl-loop for item in recall-items
             for command = (recall--item-command item)
             do (puthash command (1+ (gethash command string-count 0))
                         string-count)
             do (puthash command (eq (gethash command string-unique-p 'not-found)
                                     'not-found)
                         string-unique-p))
    (cl-loop with items = (or items recall-items)
             for item in items
             for command = (recall--item-command item)
             for count = (puthash command (1- (gethash command string-count))
                                  string-count)
             collect (cons (concat command
                                   (unless (gethash command string-unique-p)
                                     (propertize (format "<%d>" count)
                                                 'face 'shadow)))
                           item))))

(defun recall--make-annotation (alist)
  (lambda (candidate)
    (let ((item (cdr (assoc candidate alist 'string-equal))))
      ;; HACK Use `tabulated-list-mode' to create annotation
      (with-temp-buffer
        (tabulated-list-mode)
        (setq tabulated-list-format recall-list-format)
        (let ((item (copy-tree item)))
          (setf (recall--item-command item) "")
          (setq-local recall-items (list item)))
        (add-hook 'tabulated-list-revert-hook 'recall--list-refresh nil t)
        (revert-buffer)
        (string-trim-right (buffer-string))))))

(defun recall-completing-read (prompt &optional predicate)
  "Read a command string in the minibuffer, with completion.
PROMPT is a command to prompt with; normally it ends in a colon and
a space.  PREDICATE is an optional function taking the item command
and `recall--item'.
Completes from collection based on `recall-items'."
  (let* ((alist (recall--collection))
         (collection
          (lambda (command predicate action)
            (cond
             ((eq action 'metadata)
              `(metadata
                (category . recall)
                (annotation-function . ,(recall--make-annotation alist))
                (display-sort-function . identity)))
             (t
              (complete-with-action action alist command predicate)))))
         ;; Properties are added in `recall--collection'.
         ;; Therefore we need the properties intact to get `equal' to
         ;; match in `assoc'.
         (minibuffer-allow-text-properties t))
    (cdr (assoc (completing-read prompt collection predicate t) alist))))


;;; Items
(defun recall-save ()
  "Prune and save `recall-items'.
The history is save to `recall-save-file'.
See `recall-prune-keep-unique' and `recall-prune-after'
for pruning options."
  (interactive)
  (recall--prune)
  (with-temp-buffer
    (insert
     (concat
      ";; -*- mode: emacs-lisp; coding: utf-8-unix -*-\n"
      ";; recall history filed, automatically generated by `recall'.\n"
      "\n"))
    (let ((print-length nil)
	  (print-level nil)
	  (print-quoted t))
      (prin1
       `(setq recall-items
	      ',(mapcar (lambda (item)
                          ;; Some cleanup
                          (unless (numberp (recall--item-exit-code item))
                            (setf (recall--item-exit-code item) -1))
                          (unless (recall--item-end-time item)
                            (setf (recall--item-end-time item) (current-time)))
                          ;; Drop `process' from item
                          (take (1- (length (cl-struct-slot-info 'recall--item)))
                                item))
                        recall-items))
       (current-buffer)))
    ;; Write to `recall-save-file'
    (let ((file-precious-flag t)
	  (coding-system-for-write 'utf-8-unix)
          (dir (file-name-directory recall-save-file)))
      (unless (file-exists-p dir)
        (make-directory dir t))
      (write-region (point-min) (point-max) recall-save-file nil
		    (unless (called-interactively-p 'interactive) 'quiet)))))

(defun recall-find-log (item)
  "View log for command ITEM."
  (interactive
   (list (funcall recall-completing-read-fn "View log: ")))
  (if-let ((buffer
            (cl-find item
                     (buffer-list)
                     :key (lambda (buffer)
                            (with-current-buffer buffer recall--item)))))
      (pop-to-buffer buffer)
    (find-file (recall--log-file item))
    (recall-log-mode)))

(recall--def-do-command recall-do-find-log recall-find-log
  "View log for this command item.")

(defun recall-buffer (item)
  "View buffer for command ITEM."
  (interactive
   (list (funcall recall-completing-read-fn "View process buffer: ")))
  (let ((process (recall--item-process item)) buffer)
    (unless (processp process)
      (user-error "No process associated with `item'"))
    (setq buffer (process-buffer process))
    (unless (and (bufferp buffer) (buffer-live-p buffer))
      (user-error "Buffer killed"))
    (cl-loop
     for item in recall-items
     until (eq item item)
     for other-process = (recall--item-process item)
     when (and (processp other-process)
               (eq (process-buffer other-process)
                   buffer))
     do (user-error "Other process %s using buffer" other-process))
    (pop-to-buffer buffer)))

(recall--def-do-command recall-do-buffer recall-buffer
  "View buffer for this item.")

(defun recall-rerun (item)
  "Rerun command ITEM."
  (interactive
   (list (funcall recall-completing-read-fn "Rerun command: ")))
  (let ((default-directory (recall--item-directory item))
        (command (recall--item-command item))
        (recall--parent-condition (recall--item-condition item)))
    (funcall (cdr (or (assoc recall--parent-condition recall-rerun-alist)
                      (assoc nil recall-rerun-alist)))
             command)
    (message "Running %S" command)))

(recall--def-do-command recall-do-rerun recall-rerun
  "Rerun this command item.")

(defun recall-rerun-edit (item)
  "Edit and rerun command ITEM."
  (interactive
   (list (funcall recall-completing-read-fn "Rerun edit command: ")))
  (let ((default-directory (recall--item-directory item))
        (command (recall--item-command item))
        (recall--parent-condition (recall--item-condition item)))
    (minibuffer-with-setup-hook
        (lambda ()
          (delete-region (minibuffer-prompt-end) (point-max))
          (insert command))
      (call-interactively (cdr (or (assoc recall--parent-condition recall-rerun-alist)
                                   (assoc nil recall-rerun-alist)))
                          command))))

(recall--def-do-command recall-do-rerun-edit recall-rerun-edit
  "Edit and rerun this command item.")

(defun recall-process-kill (item)
  "Kill process of command ITEM."
  (interactive
   (list (funcall recall-completing-read-fn "Kill process: "
                  (pcase-lambda (`(_ . ,item))
                    (ignore-errors (process-live-p (recall--item-process item)))))))
  (let ((process (recall--item-process item)))
    (unless process
      (user-error "Current history item does not have an live process"))
    (kill-process process)))

(recall--def-do-command recall-do-process-kill recall-process-kill
  "Kill the process of this command item.")

(defun recall-copy-as-kill-command (item)
  "Copy the command string of command ITEM."
  (interactive
   (list (funcall recall-completing-read-fn "Copy command: ")))
  (kill-new (recall--item-command item)))

(recall--def-do-command recall-do-copy-as-kill-command recall-copy-as-kill-command
  "Copy as kill the command string of this command item.")

(defun recall-delete (item)
  "Delete command ITEM from `recall-items'."
  (interactive
   (list (funcall recall-completing-read-fn "Delete history: ")))
  (when (or (not (called-interactively-p 'all))
            (yes-or-no-p "Are you sure you want to delete command?"))
    (delete-file (recall--log-file item))
    (setq recall-items (delq item recall-items))))

(recall--def-do-command recall-do-delete recall-delete
  "Delete this command item from `recall-items'.")


;;; Embark integration
(defvar embark-general-map)
(defvar embark-keymap-alist)
(defvar embark-exporters-alist)
(defvar embark-target-finders)

(defun recall-export (candidates)
  (let ((alist (recall--collection)))
    (recall-list (mapcar (lambda (cand)
                           (cdr (assoc cand alist)))
                         candidates))))

(defun recall-embark-target-finder ()
  (when (derived-mode-p 'recall-list-mode)
    `(recall ,(aref (tabulated-list-get-entry) 0)
             ,(line-beginning-position)
             . ,(1- (line-beginning-position 2)))))

(with-eval-after-load 'embark
  (defvar-keymap embark-recall-actions-map
    :doc "Recall actions"
    :parent embark-general-map
    "b" #'recall-buffer
    "f" #'recall-find-log
    "k" #'recall-process-kill
    "r" #'recall-rerun
    "e" #'recall-rerun-edit
    "w" #'recall-copy-as-kill-command
    "d" #'recall-delete)

  (add-to-list 'embark-keymap-alist '(recall . embark-recall-actions-map))
  (add-to-list 'embark-exporters-alist '(recall . recall-export))
  (add-to-list 'embark-target-finders #'recall-embark-target-finder))


;;; Consult integration
(defvar consult--annotate-align-width)
(declare-function consult--project-root "consult" ())
(declare-function consult--multi "consult" (sources &rest options))

(defun recall-consult-completing-read (prompt &optional predicate)
  "Read a command string in the minibuffer, with completion.
PROMPT is a command to prompt with; normally it ends in a colon and a
space.
PREDICATE is an optional function taking item command and
`recall--item'.
Completes from collection based on `recall-items'."
  (let* ((alist
          (cl-remove-if-not (or predicate 'identity) (recall--collection)))
         (annotate-fn-1
          (recall--make-annotation alist))
         (annotate-fn (lambda (cand)
                        ;; HACK Don't slide annotations of the edge of
                        ;; the world because of one long string.
                        (setq consult--annotate-align-width 0)
                        (funcall annotate-fn-1 cand)))
         (directory (abbreviate-file-name default-directory))
         (sources
          `(( :name "Active" :narrow ?a :items
              ,(lambda ()
                 (cl-loop for (str . item) in alist
                          unless (recall--item-exit-code item)
                          collect str)))
            ( :name "Exited" :narrow ?e :items
              ,(lambda ()
                 (cl-loop for (str . item) in alist
                          when (recall--item-exit-code item)
                          collect str)))
            ( :name "Unique" :narrow ?u :hidden t :items
              ,(lambda ()
                 (cl-loop with table = (make-hash-table :test 'equal)
                          for (str . item) in alist
                          for key = (cons (recall--item-command item)
                                          (recall--item-directory item))
                          unless (gethash key table) collect
                          str and do (puthash key t table))))
            ( :name "Project" :narrow ?p :hidden t :items
              ,(lambda ()
                 (when-let* ((root (consult--project-root))
                             (root (abbreviate-file-name root)))
                   (cl-loop for (str . item) in alist
                            when (equal root (recall--item-directory item))
                            collect str))))
            ( :name ,(format "Directory (%s)" directory) :narrow ?d :hidden t :items
              ,(lambda ()
                 (cl-loop for (str . item) in alist
                          when (equal directory (recall--item-directory item))
                          collect str)))))
         (sources
          (cl-loop for source in sources collect
                   (append source
                           `(:category recall :annotate ,annotate-fn))))
         (match (car (consult--multi sources
                                     :prompt prompt
                                     :require-match t
                                     :sort nil))))
    (alist-get match alist nil nil 'equal)))

(with-eval-after-load 'consult
  (setq recall-completing-read-fn #'recall-consult-completing-read))


;;; Mode

;;;###autoload
(define-minor-mode recall-mode
  "Recall Emacs subprocesses."
  :global t
  (if recall-mode
      (progn
        (advice-add 'make-process :around 'recall--make-process)
        (advice-add 'set-process-filter :around 'recall--set-process-filter)
        (advice-add 'set-process-sentinel :around 'recall--set-process-sentinel)
        (add-hook 'kill-emacs-hook 'recall-save)
        (when (and (file-exists-p recall-save-file) (not recall-items))
          (load recall-save-file t)))
    (advice-remove 'make-process 'recall--make-process)
    (advice-remove 'set-process-filter 'recall--set-process-filter)
    (advice-remove 'set-process-sentinel 'recall--set-process-sentinel)
    (remove-hook 'kill-emacs-hook 'recall-save)))

(provide 'recall)
;;; recall.el ends here

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:
