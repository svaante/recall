;;; process-history.el --- Extensive history for processes -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Daniel Pettersson
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.0.2
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

;; This package `advice-add's `make-process' and friends to store
;; metadata like; working directory, stdout, start time, end time,
;; exit code and version control revision for any Emacs sub-processes.

;; It defines commands to view and manage both currently running
;; processes and historical processes, accessible via the minibuffer
;; and a specialized `tabulated-list' buffer.

;; Joining the functionality of bash reverse-i-search with `proced'.

;; Enable process surveillance with `process-history-mode'.

;; Integration with consult and embark are found in extensions/*.el

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
(require 'esh-mode)


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
    (major-mode . shell-command-mode)
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
    project-shell-command
    project-async-shell-command)
  "Which commands to enable process history for.
This option works in union with `process-history-buffer-match'."
  :type '(repeat function))

(defcustom process-history-rerun-alist
  '((nil . async-shell-command)
    ((major-mode . compilation-mode) . compile))
  "Rerun command from ALIST specification (CONDITION . FN).
Where condition is either an item in `process-history-buffer-match',
`process-history-this-command' or nil for anything else."
  :type 'alist)

(defcustom process-history-prune-after (* 60 60 24 7 2) ;; two weeks
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
  '(("Command" . --item-command)
    ("RC" .
     (lambda (item)
       (when-let ((code (--item-exit-code item)))
         (propertize
          (format "%s" code) 'face
          (cond
           ((and-let* ((process (--item-process item)))
              (process-live-p process))
            'default)
           ((equal (--item-exit-code item) 0)
            'process-history-success-face)
           (t 'process-history-error-face))))))
    ("Start" .
     (lambda (item)
       (propertize (--format-time (--item-start-time item))
                   'face 'process-history-time-face)))
    ("Time" . (lambda (item)
                (propertize
                 (--relative-time
                  (- (time-to-seconds (--item-end-time item))
                     (time-to-seconds (--item-start-time item))))
                 'face 'process-history-time-face)))
    ("Directory" . (lambda (item)
                     (propertize
                      (directory-file-name (--item-directory item))
                      'face 'process-history-directory-face)))
    ("VC" . (lambda (item)
              (when-let ((vc (--item-vc item)))
                (propertize vc 'face 'process-history-vc-face))))
    ("Buffer" . (lambda (item)
                  (let ((process (--item-process item)))
                    (when (process-live-p process)
                      (format "%s" (process-buffer process))))))
    ("PID" . (lambda (item)
               (ignore-errors
                 (format "%s" (process-id (--item-process item)))))))
  "Log item format alist.
Alist of (NAME . FN) pairs.  Where FN takes `process-history--item' should
return string."
  :type 'alist)

(defcustom process-history-list-format
  (vector '("Command" 80 t)
          '("RC" 4 t :right-align t)
          '("Start" 13 t :right-align t)
          '("Time" 8 t :right-align t)
	  '("Directory" 35 t :right-align t)
          '("VC" 8 t)
          '("Buffer" 25 t)
          '("PID" 8 t))
  "See `tabulated-list-format'.
Each NAME needs to exist in `process-history-format-alist' to be
displayed correctly."
  :type 'vector)

(defcustom process-history-completing-read-fn
  #'process-history-completing-read
  "Function used to complete process history candidates.
See `process-history-completing-read'."
  :type 'function)


;;; Faces
(defface process-history-directory-face
  '((t :inherit dired-directory))
  "Face used in Directory column.")

(defface process-history-time-face
  '((t :inherit font-lock-doc-face))
  "Face used to highlight time.")

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
  (cond
   ((eq this-command 'eshell-send-input)
    ;; HACK Use un-expanded paths in `eshell'
    (buffer-substring-no-properties
     eshell-last-input-start
     (1- eshell-last-input-end)))
   (t
    (string-trim
     (string-join (if (equal (nth 1 command) shell-command-switch)
                      (nthcdr 2 command)
                    command)
                  " ")))))

(defun --log-file (item)
  (file-name-concat process-history-directory
                    (format-time-string process-history-log-format
                                        (--item-start-time item))))

(defun --relative-time (diff)
  (cond
   ((> diff (* 24 60 60)) (format-seconds "%dd %hh" diff))
   ((> diff (* 60 60)) (format-seconds "%hh %mm" diff))
   (t (format-seconds "%mm %ss%z" diff))))

(defun --format-time (time)
  (let ((diff (- (time-to-seconds nil)
                 (time-to-seconds time))))
    (if (> diff (* 7 24 60 60))
        (format-time-string "%b %d %H:%M" time)
      (format "%s ago" (--relative-time diff)))))

(defun --prune ()
  (setq process-history
        (cl-loop with command-set = (make-hash-table :test 'equal)
                 for item in process-history
                 for command = (--item-command item)
                 ;; An history item is unique if another item
                 ;; shares command string.
                 for key = command
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
                 else do (delete-file (--log-file item)))))

(defmacro --def-do-command (name command doc)
  (declare (indent 2))
  `(defun ,name ()
     ,doc
     (interactive)
     (let ((item (cond
                  ((derived-mode-p 'process-history-list-mode)
                   (tabulated-list-get-id))
                  ((derived-mode-p 'process-history-log-mode)
                   --log-item)))
           (buffer (current-buffer)))
       (,command item)
       (accept-process-output (--item-process item) 0.2)
       (with-current-buffer buffer
         (when (eq major-mode 'process-history-list-mode)
           (revert-buffer nil nil t)
           (forward-line))))))


;;; Latch on `make-process'
(defvar --condition nil)

(defun --make-process (make-process &rest args)
  (let (command directory buffer condition)
    (cond
     ((and
       ;; Skip tramp process
       (not (equal signal-hook-function 'tramp-signal-hook-function))
       (setq command (--command-to-string (plist-get args :command))
             directory (or (plist-get args :directory) default-directory)
             buffer (let ((buffer (plist-get args :buffer)))
                      (pcase buffer
                        ((pred bufferp) buffer)
                        ((pred stringp) (get-buffer buffer))
                        (_  (current-buffer)))))
       (not (string-empty-p command))
       (setq condition
             (or
              ;; Injected from `process-history-rerun'
              --condition
              (cl-find this-command
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
        (write-region string nil log-file 'append 'no-echo)
        (when-let ((buffer (get-file-buffer log-file)))
          (with-current-buffer buffer
            (revert-buffer nil t t)))))))

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
  (setq tabulated-list-entries
        (cl-loop for item in (or process-history-local
                                 process-history)
                 for desc =
                 (cl-loop for (col) across tabulated-list-format
                          for fn = (cdr (assoc col process-history-format-alist))
                          collect (or (funcall fn item) "--") into desc
                          finally return (apply 'vector desc))
                 collect (list item desc))))

(defvar-keymap process-history-list-mode-map
  :doc "Local keymap for `process-history-list-mode' buffers."
  :parent tabulated-list-mode-map
  "C-m"           #'process-history-do-find-log
  "D"             #'process-history-do-process-kill
  "r"             #'process-history-do-rerun
  "o"             #'process-history-do-buffer
  "w"             #'process-history-do-copy-as-kill-command
  "d"             #'process-history-do-delete-item
  "<mouse-2>"     #'process-history-do-find-log
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
    (pop-to-buffer buffer)))


;;; Logs
(defvar --log-filter-functions nil)

(defvar-local --log-item nil)

(defvar-keymap process-history-log-mode-map
  :doc "Local keymap for `process-history-mode' buffers."
  "r"             #'process-history-do-rerun
  "w"             #'process-history-do-copy-as-kill-command
  "d"             #'process-history-do-delete-item)

(define-derived-mode process-history-log-mode special-mode "Log"
  "Mode active in `process-history' log files."
  ;; TODO Auto revert overlay info
  (setq --log-item
        (cl-find-if (lambda (item)
                      (equal (--log-file item) buffer-file-name))
                    process-history))
  (unless --log-item
    (user-error "Unable find connection with log file %s in `process-history'"
                buffer-file-name))
  (unless (file-exists-p buffer-file-name)
    (let ((inhibit-read-only t))
      (save-excursion
        (insert (propertize "* Log file has been deleted *"
                            'face 'warning)))))
  (let ((overlay
         (or (cl-find 'process-history-log-overlay
                      (overlays-in (point-min) (point-max))
                      :key (lambda (ov) (overlay-get ov 'category)))
             (make-overlay (point-min) (point-min)))))
    (overlay-put overlay 'category 'process-history-log-overlay)
    (overlay-put overlay 'before-string
                 (cl-loop
                  with max-length =
                  (apply 'max (mapcar (lambda (x) (length (car x)))
                                      process-history-format-alist))
                  for (name . accessor) in process-history-format-alist
                  for value = (funcall accessor --log-item)
                  when value concat
                  (format (format "%%%ds: %%s\n" max-length) name value)
                  into before-string
                  finally return
                  (concat (propertize before-string
                                      'face 'process-history-log-overlay-face)
                          "\n"))))
  (setq buffer-file-name nil
        default-directory (--item-directory --log-item))
  (rename-buffer (format "*Log %S*" (--item-command --log-item)) t)
  (let ((inhibit-read-only t))
    (run-hooks '--log-filter-functions)))

(defun --log-asni-color-filter ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-hook '--log-filter-functions #'--log-asni-color-filter)


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

(defun --make-annotation (alist)
  (lambda (candidate)
    (let ((item (cdr (assoc candidate alist 'string-equal))))
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
                (annotation-function . ,(--make-annotation alist))
                (display-sort-function . identity)))
             (t
              (complete-with-action action alist string predicate)))))
         ;; Properties are added in `--collection'.
         ;; Therefore we need the properties intact to get `equal' to
         ;; match in `assoc'.
         (minibuffer-allow-text-properties t))
    (cdr (assoc (completing-read prompt collection predicate t) alist))))


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
  (interactive
   (list (funcall process-history-completing-read-fn "View log: ")))
  (if-let ((buffer
            (cl-find history-item
                     (buffer-list)
                     :key (lambda (buffer)
                            (with-current-buffer buffer --log-item)))))
      (pop-to-buffer buffer)
    (find-file (--log-file history-item))
    (process-history-log-mode)))

(--def-do-command process-history-do-find-log process-history-find-log
  "View log for this item.")

(defun process-history-buffer (history-item)
  "View buffer for HISTORY-ITEM."
  (interactive
   (list (funcall process-history-completing-read-fn "View process buffer: ")))
  (let ((process (--item-process history-item)) buffer)
    (unless (processp process)
      (user-error "No process associated with `history-item'"))
    (setq buffer (process-buffer process))
    (unless (and (bufferp buffer) (buffer-live-p buffer))
      (user-error "Buffer killed"))
    (cl-loop
     for item in process-history
     until (eq item history-item)
     for other-process = (--item-process item)
     when (and (processp other-process)
               (eq (process-buffer other-process)
                   buffer))
     do (user-error "Other process %s using buffer" other-process))
    (pop-to-buffer buffer)))

(--def-do-command process-history-do-buffer process-history-buffer
  "View buffer for this item.")

(defun process-history-find-dwim (history-item)
  "View buffer or log associated with HISTORY-ITEM."
  (interactive
   (list (funcall process-history-completing-read-fn "View log or buffer: ")))
  (let ((process (--item-process history-item)))
    (if (and (processp process) (process-live-p process))
        (process-history-buffer history-item)
      (process-history-find-log history-item))))

(defun process-history-rerun (history-item)
  "Rerun command from HISTORY-ITEM."
  (interactive
   (list (funcall process-history-completing-read-fn "Rerun command: ")))
  (let ((default-directory (--item-directory history-item))
        (command (--item-command history-item))
        (--condition (--item-condition history-item)))
    (funcall (cdr (or (assoc --condition process-history-rerun-alist)
                      (assoc nil process-history-rerun-alist)))
             command)
    (message "Running %S" command)))

(--def-do-command process-history-do-rerun process-history-rerun
  "Rerun command of this item.")

(defun process-history-process-kill (history-item)
  "Kill HISTORY-ITEMs process."
  (interactive
   (list (funcall process-history-completing-read-fn "Kill process: "
                  (pcase-lambda (`(_ . ,item))
                    (ignore-errors (process-live-p (--item-process item)))))))
  (let ((process (--item-process history-item)))
    (unless process
      (user-error "Current history item does not have an live process."))
    (kill-process process)))

(--def-do-command process-history-do-process-kill process-history-process-kill
  "Kill process of this item.")

(defun process-history-copy-as-kill-command (history-item)
  "Copy command of HISTORY-ITEM."
  (interactive
   (list (funcall process-history-completing-read-fn "Copy command: ")))
  (kill-new (--item-command history-item)))

(--def-do-command process-history-do-copy-as-kill-command
    process-history-copy-as-kill-command
  "Copy as kill the command of this item.")

(defun process-history-delete-item (history-item)
  "Delete HISTORY-ITEM."
  (interactive
   (list (funcall process-history-completing-read-fn "Delete history: ")))
  (when (or (not (called-interactively-p 'all))
            (yes-or-no-p "Are you sure you want to delete history item?"))
    (delete-file (--log-file history-item))
    (setq process-history (delq history-item process-history))))

(--def-do-command process-history-do-delete-item
    process-history-delete-item
  "Remove this item from `process-history'.")


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
;; checkdoc-force-docstrings-flag: nil
;; End:
