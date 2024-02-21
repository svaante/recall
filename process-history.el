;;; process-history.el --- Extensive history for processes -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Daniel Pettersson
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.0.1
;; Homepage: https://github.com/svaante/process-history
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

;; This package advices `make-process' and friends to store command,
;; working directory, stdout, start time, end time, exit code and vc
;; revision for emacs sub-processes.
;; Think .bash_history++.

;; Enable process surveillance with `process-history-mode'.

;; Note:
;; As this package advices core functionality, usage might have
;; unintended consequences.  Disable `process-history-mode' at the
;; first signs of process spawning troubles.

;; Package takes some inspiration from the excellent package
;; detached.el

;;; Code:

(require 'cl-macs)
(require 'tramp)
(require 'tabulated-list)


;;; Custom
(defgroup process-history nil
  "Extensive history for processes."
  :prefix "process-history-"
  :group 'applications)

(defcustom process-history-directory "/tmp/"
  "Where to place logs."
  :type 'directory)

(defcustom process-history-save-file
  (locate-user-emacs-file "process-history")
  "File to store history."
  :type 'file)

(defcustom process-history-buffer-match
  '((major-mode . compilation-mode)
    (major-mode . eshell-mode))
  "Add history for additional process buffers.
See `buffer-match-p'.
This option works in union with `process-history-this-command'."
  :type '(repeat (choice :tag "Condition"
			 regexp
			 (function :tag "Matcher function"))))

(defcustom process-history-this-command
  '(shell-command
    async-shell-command
    process-history-rerun-with-compile
    process-history-rerun-with-async-shell-command)
  "Which commands to enable process history for.
This option works in union with `process-history-buffer-match'."
  :type '(repeat function))

(defcustom process-history-prune-after (* 60 24 7 3)
  "Prune history after seconds on `process-history-save'."
  :type '(choice (natnum :tag "Prune history item after seconds.")
                 (const :tag "Never prune." nil)))

(defcustom process-history-prune-keep-unique t
  "Keep uniqe commands in history.'"
  :type 'boolean)

(defcustom process-history-timestmap-format
  "%Y-%02m-%02d %02H:%02M:%02S"
  "Format of timestamp annotation .
See `time-stamp-format'."
  :type 'string)

(defcustom process-history-seconds-format
  "0s%x%yy %dd %hh %mm %ss%z"
  "Format of active time annoation.
See `format-seconds'."
  :type 'string)

(defcustom process-history-log-format
  "%Y-%02m-%02d_%02H_%02M:%02S_%N.log"
  "Log file format.
Warning: `process-history' won't find old logs if changed.
See `time-stamp-format'."
  :type 'string)

(defcustom process-history-log-header-alist
  `((command . --item-command)
    (directory . --item-directory)
    (exit-code . --item-exit-code)
    (start-time . ,(apply-partially '--time-format-slot 'start-time))
    (end-time . ,(apply-partially '--time-format-slot 'end-time))
    (vc . --item-vc)
    (condition . --item-condition))
  "Log header specification.
Alist of (NAME . ACCESSOR-FN)."
  :type 'alist)

;;; Faces
(defface process-history-directory-face
  '((t :inherit dired-directory))
  "Face used in Directory column.")

(defface process-history-success-face
  '((t :inherit success))
  "Face used in Code column for success status.")

(defface process-history-error-face
  '((t :inherit error))
  "Face used in Code column for error status.")

(defface process-history-vc-face
  '((t :inherit shadow))
  "Face used in VC column.")

(defface process-history-condition-face
  '((t :inherit italic))
  "Face used in Condition column.")

(defface process-history-log-overlay-face
  '((t :inherit font-lock-comment-face :extend t))
  "Face used in `process-history-log-mode' info overlay.")


;;; Global vars
(defvar process-history nil
  "History list of `process-history--item' items.")


;;; Data
(cl-defstruct (--item (:type list))
  command directory exit-code start-time end-time vc condition process)


;;; Utils
(defun --command-to-string (command)
  (string-join (if (equal (nth 1 command) shell-command-switch)
                   (nthcdr 2 command)
                 command)
               " "))

(defun --log-file (item)
  (file-name-concat process-history-directory
                    (format-time-string process-history-log-format
                                        (--item-start-time item))))

(defun --time-format-slot (slot item)
  (format-time-string process-history-timestmap-format
                      (nth (cl-struct-slot-offset '--item slot) item)))

(defun --prune ()
  (setq process-history
        (cl-loop with command-set = (make-hash-table :test 'equal)
                 for item in process-history
                 for command = (--item-command item)
                 for unique-command-p = (not (gethash command command-set))
                 do (puthash command t command-set)
                 if (or (not process-history-prune-after)
                        (< (- (time-to-seconds)
                                (time-to-seconds
                                 (--item-start-time item)))
                           process-history-prune-after)
                        (and process-history-prune-keep-unique
                             unique-command-p))
                 collect item
                 else
                 do (delete-file (--log-file item)))))


;;; Latch on `make-process'
(defun --make-process (make-process &rest args)
  (let ((command
         (--command-to-string (plist-get args :command)))
        (directory (or (plist-get args :directory) default-directory))
        (buffer (or (plist-get args :buffer) (current-buffer)))
        condition)
    (cond
     ((and
       ;; TODO Disabled for tramp as there is no world where it works
       (not (tramp-tramp-file-p directory))
       (not (string-empty-p (string-trim command)))
       (setq condition
             (or (cl-find this-command
                          process-history-this-command)
                 (cl-find-if (lambda (condition)
                               (buffer-match-p condition buffer))
                             process-history-buffer-match))))
      (let ((item
             (make-process-history--item
              :command command
              :start-time (current-time)
              :directory (abbreviate-file-name directory)
              :condition condition
              ;; FIXME Should be possible to support other vc backends
              ;; TODO Would be nice if we could store dirty, clean etc.
              :vc (vc-working-revision (file-name-as-directory directory) 'Git))))
        (plist-put args :filter
                   (--make-filter item (plist-get args :filter)))
        (plist-put args :sentinel
                   (--make-sentinel item (plist-get args :sentinel)))
        (push item process-history)
        (setf (--item-process item) (apply make-process args))))
     (t
      (apply make-process args)))))

(defun --set-process-filter (set-filter process filter)
  (if-let ((item (car (cl-member process
                                 process-history
                                 :key '--item-process))))
      (funcall set-filter process (--make-filter item filter))
    (funcall set-filter process filter)))

(defun --set-process-sentinel (set-sentinel process sentinel)
  (if-let ((item (car (cl-member process
                                 process-history
                                 :key '--item-process))))
      (funcall set-sentinel process
               (--make-sentinel item sentinel))
    (funcall set-sentinel process sentinel)))

(defun --make-filter (item filter)
  (let ((log-file (--log-file item))
        (filter (or filter 'ignore)))
    (lambda (proc string)
      (unwind-protect
          (funcall filter proc string)
        (write-region string nil log-file 'append 'no-echo)))))

(defun --make-sentinel (item sentinel)
  (let ((sentinel (or sentinel 'ignore)))
    (lambda (proc msg)
      (unwind-protect
          (funcall sentinel proc msg)
        (when (memq (process-status proc) '(exit signal))
          (setf (--item-end-time item)
                (current-time)
                (--item-exit-code item)
                (process-exit-status proc)))))))


;;; List
(defun process-history--list-refresh ()
  ;; TODO Should be able to filter by column value
  (setq tabulated-list-entries
        (cl-loop
         for item in process-history
         collect
         (list item
               `[;; Command
                 ,(--item-command item)
                 ;; Directory
                 ,(propertize (--item-directory item)
                              'face 'process-history-directory-face)
                 ;; Start
                 ,(--time-format-slot 'start-time item)
                 ;; Code
                 ,(if-let ((code (--item-exit-code item)))
                      (propertize (format "%s" code)
                                  'face
                                  (cond
                                   ((and-let* ((process (--item-process item)))
                                      (process-live-p process))
                                    'default)
                                   ((equal (--item-exit-code item) 0)
                                    'process-history-success-face)
                                   (t 'process-history-error-face)))
                    "*")
                 ;; Time
                 ,(format-seconds process-history-seconds-format
                                   (- (time-to-seconds
                                       (--item-end-time item))
                                      (time-to-seconds (--item-start-time item))))
                 ;; VC
                 ,(propertize (or (--item-vc item) "")
                              'face 'process-history-vc-face)
                 ;; Condition
                 ,(propertize (format "%S" (--item-condition item))
                              'face 'process-history-condition-face)]))))

(defvar-keymap process-history-list-mode-map
  :doc "Local keymap for `process-history-list-mode' buffers."
  :parent tabulated-list-mode-map
  "C-m"           #'process-history-find-log
  "C-c C-k"       #'process-history-process-kill
  "o"             #'process-history-display-buffer
  "x"             #'process-history-copy-as-kill-command
  "c"             #'process-history-rerun-with-compile
  "&"             #'process-history-rerun-with-async-shell-command
  "d"             #'process-history-delete-item
  "<mouse-2>"     #'process-history-find-log
  "<follow-link>" 'mouse-face)

(define-derived-mode process-history-list-mode tabulated-list-mode "Process History"
  "List Process History."
  :interactive nil
  (setq-local buffer-stale-function
              (lambda (&optional _noconfirm) 'fast))
  (setq tabulated-list-use-header-line t
        tabulated-list-format
        (vector '("Command" 80 t)
		'("Directory" 45 t)
                '("Start" 19 t)
                '("Code" 4 t :right-align t)
                '("Time" 8 t)
                '("VC" 8 t)
                '("Condition" 0 t)))
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'process-history--list-refresh nil t))

(add-hook 'process-history-list-mode-hook 'auto-revert-mode)


;;; Commands
(defun process-history-save ()
  "Prune and save `process-history'.
The history is save to `process-history-save-file'.
See `process-history-prune-keep-unique' and `process-history-prune-after'
for pruning options."
  (interactive)
  (--prune)
  (with-temp-buffer
    (insert
     (concat
      ";; -*- mode: emacs-lisp; coding: utf-8-unix -*-\n"
      ";; process-history history filed, automatically generated by `process-history'.\n"
      "\n"))
    (let ((print-length nil)
	  (print-level nil)
	  (print-quoted t))
      (prin1
       `(setq process-history
	      ',(mapcar (lambda (item)
                          ;; Some cleanup
                          (unless (numberp (--item-exit-code item))
                            (setf (--item-exit-code item) -1))
                          (unless (--item-end-time item)
                            (setf (--item-exit-code item) (current-time)))
                          ;; Drop `process' from item
                          (take (1- (length (cl-struct-slot-info '--item)))
                                item))
                        process-history))
       (current-buffer)))
    ;; Write to `process-history-save-file'
    (let ((file-precious-flag t)
	  (coding-system-for-write 'utf-8-unix)
          (dir (file-name-directory process-history-save-file)))
      (unless (file-exists-p dir)
        (make-directory dir t))
      (write-region (point-min) (point-max) process-history-save-file nil
		    (unless (called-interactively-p 'interactive) 'quiet)))))

(defcustom process-history-on-rerun-hooks '(quit-window)
  "Close *Process History* buffer on rerun commands."
  :type 'hook)

(defun process-history-find-log (history-item)
  "View log file for HISTORY-ITEM."
  (interactive (list (tabulated-list-get-id)) process-history-list-mode)
  (if-let ((buffer
            (cl-find-if (lambda (buffer)
                          (eq history-item (with-current-buffer buffer --log-item)))
                        (buffer-list))))
      (pop-to-buffer buffer)
    (find-file (--log-file history-item))
    (process-history-log-mode)))

(defun process-history-display-buffer (history-item)
  "View buffer for HISTORY-ITEM."
  (interactive (list (tabulated-list-get-id)) process-history-list-mode)
  (display-buffer (process-buffer (--item-process history-item))))

(defun process-history-rerun-with-compile (history-item)
  "Rerun HISTORY-ITEM with `compile'."
  (interactive (list (tabulated-list-get-id)) process-history-list-mode)
  (run-hooks 'process-history-on-rerun-hooks)
  (let ((default-directory (--item-directory history-item)))
    (compile (--item-command history-item))))

(defun process-history-rerun-with-async-shell-command (history-item)
  "Rerun HISTORY-ITEM with `async-shell-command'."
  (interactive (list (tabulated-list-get-id)) process-history-list-mode)
  (run-hooks 'process-history-on-rerun-hooks)
  (let ((default-directory (--item-directory history-item)))
    (async-shell-command (--item-command history-item))))

(defun process-history-process-kill (history-item)
  "Kill HISTORY-ITEMs process."
  (interactive (list (tabulated-list-get-id)) process-history-list-mode)
  (kill-process (--item-process history-item)))

(defun process-history-copy-as-kill-command (history-item)
  "Copy command of HISTORY-ITEM."
  (interactive (list (tabulated-list-get-id)) process-history-list-mode)
  (kill-new (--item-command history-item)))

(defun process-history-delete-item (history-item)
  "Delete HISTORY-ITEM."
  (interactive (list (tabulated-list-get-id)) process-history-list-mode)
  (when (or (not (called-interactively-p 'all))
            (yes-or-no-p "Are you sure you want to delete history item?"))
    (delete-file (--log-file history-item))
    (setq process-history (delq history-item process-history))
    (revert-buffer)))

;;;###autoload
(defun process-history-list ()
  (interactive)
  (let ((buffer (get-buffer-create "*Process History*")))
    (with-current-buffer buffer
      (process-history-list-mode)
      (auto-revert-mode)
      (revert-buffer)
      (goto-char (point-min)))
    (select-window
     (display-buffer buffer
                     '((display-buffer-at-bottom)
                       (dedicated . t))))))

(defvar-local --log-item nil)

(define-derived-mode process-history-log-mode special-mode "Log"
  "Mode active in `process-history' log files."
  :interactive nil
  (let ((item
         (cl-find-if (lambda (item)
                       (equal (--log-file item)
                              buffer-file-name))
                     process-history)))
    (unless item
      (user-error "Unable find connection with log file %s in `process-history'"
                  buffer-file-name))
    (let ((overlay
           (or (cl-find 'process-history-log-overlay
                        (overlays-in (point-min) (point-max))
                        :key (lambda (ov) (overlay-get ov 'category)))
               (make-overlay (point-min) (point-min)))))
      (overlay-put overlay 'category 'process-history-log-overlay)
      (overlay-put overlay
                   'before-string
                   (concat
                    (propertize
                     (cl-loop
                      with max-length =
                      (apply 'max (mapcar
                                   (lambda (x)
                                     (length (symbol-name (car x))))
                                   process-history-log-header-alist))
                      for (name . accessor) in process-history-log-header-alist
                      concat (format (format "%%%ds: %%s\n" max-length)
                                     name (funcall accessor item)))
                     'face 'process-history-log-overlay-face)
                    "\n")))
    (rename-buffer (format "*Log \"%s\" @ %s*"
                           (--item-command item)
                           (--time-format-slot 'start-time item))
                   t)
    (setq --log-item item
          buffer-read-only t
          buffer-file-name nil
          default-directory (--item-directory item))))

(add-hook 'process-history-log-mode-hook 'compilation-minor-mode)

;;;###autoload
(define-minor-mode process-history-mode
  "Extensive history for processes."
  :global t
  (cond
   (process-history-mode
    (advice-add 'make-process :around '--make-process)
    (advice-add 'set-process-filter :around '--set-process-filter)
    (advice-add 'set-process-sentinel :around '--set-process-sentinel)
    (add-hook 'kill-emacs-hook 'process-history-save)
    (when (and (file-exists-p process-history-save-file)
               (not process-history))
      (load process-history-save-file t)))
   (t
    (advice-remove 'make-process '--make-process)
    (advice-remove 'set-process-filter '--set-process-filter)
    (advice-remove 'set-process-sentinel '--set-process-sentinel)
    (remove-hook 'kill-emacs-hook 'process-history-save))))

(provide 'process-history)
;;; process-history.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("--" . "process-history--"))
;; End:
