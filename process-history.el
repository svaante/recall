;;; process-history.el --- Extensive history for processes -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Daniel Pettersson
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.0.1
;; Homepage: https://github.com/svaante/dape
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
;; Think .bash_history, but everything you ever wanted.

;; Enable process surveillance with `process-history-mode'.

;; Note:
;; As this package advices core functionality, usage might have
;; unintended consequences.  Disable `process-history-mode' at the
;; first signs of troubles with spawning process.

;;; Code:

(require 'cl-macs)
(require 'tramp)
(require 'project)


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
  "Which buffers to enable process history for.
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

(defcustom process-history-project-fn (lambda ()
                                        (project-root (project-current)))
  "Function which returns project root directory."
  :type 'function)

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

(defcustom process-history-annotate-alist
  `((--annotate-directory 40)
    (--annotate-active-time 20)
    (,(apply-partially '--time-format-slot 'start-time) 20)
    (--item-vc 40 'shadow))
  "Annotation formating specification.
Alist of (ACCESSOR-FN LENGTH FACE)."
  :type 'alist)


;;; Faces
(defface process-history-directory-face
  '((t :inherit dired-directory))
  "Face used to annotate directory.")

(defface process-history-success-face
  '((t :inherit success))
  "Face used to annotate process success status.")

(defface process-history-error-face
  '((t :inherit error))
  "Face used to annotate process error status.")

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
              :directory directory
              :condition condition
              ;; FIXME Should be possible to support other vc backends
              ;; TODO Would be nice if we could store dirty, clean etc.
              :vc (or (vc-working-revision (file-name-as-directory directory) 'Git) ""))))
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


;;; Annotation
(defun --annotate-directory (item)
  (propertize
   (let ((directory (--item-directory item)))
     (if (tramp-tramp-file-p directory)
         directory
       (abbreviate-file-name directory)))
   'face 'process-history-directory-face))

(defun --annotate-active-time (item)
  (propertize
   (format-seconds process-history-seconds-format
                   (- (time-to-seconds
                       (--item-end-time item))
                      (time-to-seconds (--item-start-time item))))
   'face
   (cond
    ((and-let* ((process (--item-process item)))
       (process-live-p process))
     'default)
    ((equal (--item-exit-code item) 0)
     'process-history-success-face)
    (t 'process-history-error-face))))

(defun --annotate-start-time (item)
  (format-time-string process-history-timestmap-format
                      (--item-start-time item)))


;;; Completion
(defun --collection ()
  (nreverse
   (cl-loop with command-count = (make-hash-table :test 'equal)
            for item in (reverse process-history)
            for command = (--item-command item)
            for count = (or (gethash command command-count) 0)
            do (puthash command (1+ count) command-count)
            collect (cons (concat command
                                  (unless (zerop count)
                                    (propertize (format "<%d>" count)
                                                'face 'shadow)))
                          item))))

(defun process-history-completing-read (prompt &optional predicate)
  "Read a string in the minibuffer, with completion.
PROMPT is a string to prompt with; normally it ends in a colon and a
space.
PREDICATE is an optional function taking command string and
`process-history--item'.
Completes from collection based on `process-history'."
  (let* ((max-candidate-width 80)
         (alist (--collection))
         (collection
          (lambda (string predicate action)
            (cond
             ((eq action 'metadata)
              `(metadata
                (category . process-history)
                (annotation-function
                 . ,(lambda (string)
                      (setq max-candidate-width
                            (max (length string) max-candidate-width))
                      (concat
                       (propertize " " 'display
                                   `(space :align-to
                                           (+ left ,max-candidate-width)))
                       " "
                       (let ((item (alist-get string alist
                                              nil nil 'equal)))
                         (mapconcat
                          (pcase-lambda (`(,fn ,length ,face))
                            (apply 'propertize
                                   (truncate-string-to-width
                                    (format "%s" (or (funcall fn item) ""))
                                    length 0 ?\s "...")
                                   (when face
                                     (list 'face face))))
                          process-history-annotate-alist
                          " ")))))
                (display-sort-function . identity)))
             (t
              (complete-with-action action
                                    alist
                                    string
                                    predicate)))))
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

(defun process-history-find-dwim (history-item)
  "View buffer or log file for HISTORY-ITEM."
  (interactive (list (process-history-completing-read "Find process buffer or log: ")))
  (if-let* ((process (--item-process history-item))
            ((processp process))
            ((process-live-p process))
            (buffer (process-buffer process))
            ((buffer-live-p buffer))
            (buffer-process (or (get-buffer-process buffer)
                                process))
            ((eq process buffer-process)))
      (pop-to-buffer buffer)
    (process-history-find-log history-item)))

(defun process-history-find-log (history-item)
  "View log file for HISTORY-ITEM."
  (interactive (list (process-history-completing-read "Find process log: ")))
  (find-file (--log-file history-item))
  (process-history-log-mode))

(defun process-history-find-project-log (history-item)
  "View log file for HISTORY-ITEM in current project."
  (interactive
   (list
    (process-history-completing-read
     "Find process log in project: "
     (let ((root (funcall process-history-project-fn)))
       (pcase-lambda (`(_ . ,item))
         (string-prefix-p root
                          (--item-directory item)))))))
  (process-history-find-log history-item))

(defun process-history-find-log-by-vc (history-item)
  "View log for HISTORY-ITEM by vc revision."
  (interactive
   (list
    (let ((revision
           (completing-read "Revision: " (mapcar '--item-vc process-history)
                            nil t (thing-at-point 'word))))
      (process-history-completing-read
       "Find process log: " (pcase-lambda (`(_ . ,item))
                              (string-prefix-p revision (--item-vc item)))))))
  (process-history-find-log history-item))

(defun process-history-rerun-with-compile (history-item)
  "Rerun HISTORY-ITEM with `compile'."
  (interactive (list (process-history-completing-read "Rerun process with compile: ")))
  (let ((default-directory (--item-directory history-item)))
    (compile (--item-command history-item))))

(defun process-history-rerun-with-async-shell-command (history-item)
  "Rerun HISTORY-ITEM with `async-shell-command'."
  (interactive (list (process-history-completing-read
                      "Rerun with async-shell-command: ")))
  (let ((default-directory (--item-directory history-item)))
    (async-shell-command (--item-command history-item))))

(defun process-history-kill (history-item)
  "Kill active HISTORY-ITEM."
  (interactive
   (list (process-history-completing-read
          "Kill process: "
          (pcase-lambda (`(_ . ,item))
            (when-let ((process (--item-process item)))
              (and (processp process) (process-live-p process)))))))
  (kill-process (--item-process history-item)))

(defun process-history-copy-as-kill-command (history-item)
  "Copy command of HISTORY-ITEM."
  (interactive
   (list (process-history-completing-read "Copy process command: ")))
  (kill-new (--item-command history-item)))

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

    (setq buffer-read-only t
          buffer-file-name nil)))

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
