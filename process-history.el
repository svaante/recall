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
;; revision for emacs sub-processes (you want).
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
(require 'tabulated-list)
(require 'tramp)
(require 'dired-aux)


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
  "Keep uniqe commands in history."
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

(defcustom process-history-format-alist
  `(("Command" . --item-command)
    ("Directory" . ,(lambda (item)
                      (propertize (--item-directory item)
                                  'face 'process-history-directory-face)))
    ("Start" . ,(apply-partially '--time-format-slot 'start-time))
    ("Code" . ,(lambda (item)
                 (if-let ((code (--item-exit-code item)))
                     (propertize (format "%s" code)
                                 'face
                                 (cond
                                  ((and-let* ((process (--item-process item)))
                                     (process-live-p process))
                                   'default)
                                  ((equal (--item-exit-code item) 0)
                                   'process-history-success-face)
                                  (t 'process-history-error-face)))
                   "*")))
    ("Time" . ,(lambda (item)
                 (format-seconds process-history-seconds-format
                                 (- (time-to-seconds
                                     (--item-end-time item))
                                    (time-to-seconds (--item-start-time item))))))
    ("VC" . ,(lambda (item) (propertize (or (--item-vc item) "")
                                        'face 'process-history-vc-face)))
    ("PID" . ,(lambda (item)
                (condition-case nil
                    (format "%s" (process-id (--item-process item)))
                  (error  "")))))
  "Log item format alist.
Alist of (NAME . FN) pairs.  Where FN takes `process-history--item' should
return string."
  :type 'alist)

(defcustom process-history-list-format
  (vector '("Command" 100 t)
	  '("Directory" 45 t)
          '("Start" 19 t)
          '("Code" 4 t :right-align t)
          '("Time" 8 t :right-align t)
          '("VC" 8 t)
          '("PID" 5 t))
  "See `tabulated-list-format'.
Each NAME needs to exist in `process-history-format-alist' to be
displayed correctly."
  :type 'vector)


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
  '((t :inherit font-lock-comment-face))
  "Face used in `process-history-log-mode' info overlay.")


;;; Global vars
(defvar process-history nil
  "History list of `process-history--item' items.")


;;; Data
(cl-defstruct (--item (:type list))
  command directory exit-code start-time end-time vc condition process)


;;; Utils
(defun --command-to-string (command)
  (string-trim
   (string-join (if (equal (nth 1 command) shell-command-switch)
                    (nthcdr 2 command)
                  command)
                " ")))

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
                 for directory = (--item-directory item)
                 for command = (--item-command item)
                 ;; An history item is not unique if another item
                 ;; shares command and directory.
                 for key = (cons command directory)
                 for unique-command-p = (not (gethash key command-set))
                 do (puthash key t command-set)
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

(defun --interactive (prompt &optional predicate)
  (list (pcase major-mode
          ('process-history-list-mode (tabulated-list-get-id))
          (_ (process-history-completing-read prompt predicate)))))


;;; Latch on `make-process'
(defun --make-process (make-process &rest args)
  (let ((command
         (--command-to-string (plist-get args :command)))
        (directory (or (plist-get args :directory) default-directory))
        (buffer
         (let ((buffer (plist-get args :buffer)))
           (pcase buffer
             ((pred bufferp) buffer)
             ((pred stringp) (get-buffer buffer))
             (_  (current-buffer)))))
        condition)
    (cond
     ((and
       ;; Skip tramp process
       (not (equal signal-hook-function 'tramp-signal-hook-function))
       (not (string-empty-p command))
       (setq condition
             (or (cl-find this-command
                          process-history-this-command)
                 (cl-find-if (lambda (condition)
                               (and (bufferp buffer)
                                    (buffer-match-p condition buffer)))
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
        ;; `auto-revert-tail-mode' needs an file to exist
        (dired-create-empty-file (--log-file item))
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
        (filter (or filter
                    (lambda (proc string)
                      (with-current-buffer (process-buffer proc)
                        (save-excursion
                          (goto-char (point-max))
                          (let ((inhibit-read-only t))
                            (insert string))))))))
    (lambda (proc string)
      (unwind-protect
          (funcall filter proc string)
        ;; TODO Allow for filter function hooks like
        ;;      `comint-output-filter-functions'.
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
(defvar-local process-history-local nil)

(defun --list-refresh ()
  ;; TODO Should be able to filter by column value
  (setq tabulated-list-entries
        (cl-loop for item in (or process-history-local
                                 process-history)
                 collect (list item
                               (cl-map 'vector
                                       (lambda (col)
                                         (funcall (cdr (assoc col process-history-format-alist))
                                                  item))
                                       (cl-mapcar 'car tabulated-list-format))))))

(defvar-keymap process-history-list-mode-map
  :doc "Local keymap for `process-history-list-mode' buffers."
  :parent tabulated-list-mode-map
  "C-m"           #'process-history-find-log
  "d"             #'process-history-process-kill
  "o"             #'process-history-display-buffer
  "x"             #'process-history-copy-as-kill-command
  "c"             #'process-history-rerun-with-compile
  "&"             #'process-history-rerun-with-async-shell-command
  "!"             #'process-history-rerun-with-async-shell-command
  "r"             #'process-history-delete-item
  "<mouse-2>"     #'process-history-find-log
  "<follow-link>" 'mouse-face)

(defvar process-history-list-mode nil)

(define-derived-mode process-history-list-mode tabulated-list-mode "Process History"
  "List Process History."
  :interactive nil
  (setq-local buffer-stale-function (lambda (&optional _noconfirm) 'fast)
              process-history-list-mode t)
  (setq tabulated-list-use-header-line t
        tabulated-list-format process-history-list-format)
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook '--list-refresh nil t))

(add-hook 'process-history-list-mode-hook 'auto-revert-mode)

;;;###autoload
(defun process-history-list (&optional items)
  "Display an list of process history items.
If ITEMS is non nil display all items."
  (interactive)
  (let ((buffer (get-buffer-create "*Process History*")))
    (with-current-buffer buffer
      (process-history-list-mode)
      (setq process-history-local items)
      (revert-buffer)
      (goto-char (point-min)))
    (select-window
     (display-buffer buffer
                     '((display-buffer-reuse-mode-window display-buffer-at-bottom)
                       (window-height . 0.3)
                       (dedicated . t)
                       (preserve-size . (t . t)))))))

;;; Logs
(define-derived-mode process-history-log-mode special-mode "Log"
  "Mode active in `process-history' log files."
  ;; TODO Auto revert overlay info
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
      (overlay-put overlay 'before-string
                   (concat
                    (propertize
                     (cl-loop
                      with max-length =
                      (apply 'max (mapcar (lambda (x)
                                            (length (car x)))
                                          process-history-format-alist))
                      for (name . accessor) in process-history-format-alist
                      concat (format (format "%%%ds: %%s\n" max-length)
                                     name (funcall accessor item)))
                     'face 'process-history-log-overlay-face)
                    "\n")))))

(add-hook 'process-history-log-mode-hook 'turn-on-auto-revert-tail-mode)
(add-hook 'process-history-log-mode-hook 'compilation-minor-mode)


;;; Complete
(defun --collection ()
  (let ((command-count (make-hash-table :test 'equal))
        (command-unique-p (make-hash-table :test 'equal)))
    (cl-loop for item in process-history
             for command = (--item-command item)
             do (puthash command (1+ (gethash command command-count 0))
                         command-count)
             do (puthash command (eq (gethash command command-unique-p 'not-found)
                                     'not-found)
                         command-unique-p))
    (cl-loop for item in process-history
             for command = (--item-command item)
             for count = (puthash command (1- (gethash command command-count))
                                  command-count)
             collect (cons (concat command
                                   (unless (gethash command command-unique-p)
                                     (propertize (format "<%d>" count)
                                                 'face 'shadow)))
                           item))))

(defun --make-annotation-function (alist)
  (lambda (string)
    (let ((item (cdr (assoc string alist 'string-equal))))
      ;; HACK Use `tabulated-list-mode' to create annotation
      (with-temp-buffer
        (tabulated-list-mode)
        (setq tabulated-list-format process-history-list-format)
        (let ((item (copy-tree item)))
          (setf (--item-command item) "")
          (setq-local process-history (list item)))
        (add-hook 'tabulated-list-revert-hook '--list-refresh nil t)
        (revert-buffer)
        (string-trim-right (buffer-string))))))

(defun process-history-completing-read (prompt &optional predicate)
  "Read a string in the minibuffer, with completion.
PROMPT is a string to prompt with; normally it ends in a colon and a
space.
PREDICATE is an optional function taking command string and
`process-history--item'.
Completes from collection based on `process-history'."
  (let* ((alist (--collection))
         (collection
          (lambda (string predicate action)
            (cond
             ((eq action 'metadata)
              `(metadata
                (category . process-history)
                (annotation-function . ,(--make-annotation-function alist))
                (display-sort-function . identity)))
             (t
              (complete-with-action action alist string predicate)))))
         ;; Properties are added in `--collection'
         ;; Therefore we need to get back test properties to equality
         (minibuffer-allow-text-properties t))
    (alist-get (completing-read prompt collection predicate t) alist
               nil nil 'equal)))


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
                            (setf (--item-end-time item) (current-time)))
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

(defun process-history-find-log (history-item)
  "View log for HISTORY-ITEM."
  (interactive (--interactive "View log: "))
  (find-file (--log-file history-item))
  (unless (eq major-mode 'process-history-log-mode)
    (process-history-log-mode)))

(defun process-history-display-buffer (history-item)
  "View buffer for HISTORY-ITEM."
  (interactive
   (--interactive "View process buffer: "
                  (pcase-lambda (`(_ . ,item))
                    (ignore-errors (process-live-p (--item-process item))))))
  (let ((process (--item-process history-item)) buffer)
    (unless (processp process)
      (user-error "No process associated with HISTORY-ITEM"))
    (setq buffer (process-buffer process))
    (unless (and (bufferp buffer) (buffer-live-p buffer))
      (user-error "Buffer killed"))
    (display-buffer buffer)))

(defun process-history-rerun-with-compile (history-item)
  "Rerun HISTORY-ITEM with `compile'."
  (interactive (--interactive "Rerun with `compile': "))
  (let ((default-directory (--item-directory history-item)))
    (compile (--item-command history-item))))

(defun process-history-rerun-with-async-shell-command (history-item)
  "Rerun HISTORY-ITEM with `async-shell-command'."
  (interactive (--interactive "Rerun with `async-shell-command': "))
  (let ((default-directory (--item-directory history-item)))
    (async-shell-command (--item-command history-item))))

(defun process-history-process-kill (history-item)
  "Kill HISTORY-ITEMs process."
  (interactive
   (--interactive "Kill process: "
                  (pcase-lambda (`(_ . ,item))
                    (ignore-errors (process-live-p (--item-process item))))))
  (let ((process (--item-process history-item)))
    (unless process
      (user-error "Current history item does not have an live process."))
    (kill-process process)))

(defun process-history-copy-as-kill-command (history-item)
  "Copy command of HISTORY-ITEM."
  (interactive (--interactive "Copy command: "))
  (kill-new (--item-command history-item)))

(defun process-history-delete-item (history-item)
  "Delete HISTORY-ITEM."
  (interactive (--interactive "Delete history: "))
  (when (or (not (called-interactively-p 'all))
            (yes-or-no-p "Are you sure you want to delete history item?"))
    (delete-file (--log-file history-item))
    (setq process-history (delq history-item process-history))
    (revert-buffer)))


;;; Advice

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
