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

;;; Code:

(require 'cl-macs)
(require 'tramp)


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

(defcustom process-history-modes
  '(compilation-mode eshell-mode)
  "Which `major-mode' to enable process history for.
This option works in union with `process-history-this-command'."
  :type '(repeat function))

(defcustom process-history-this-command
  '(shell-command
    async-shell-command
    process-history-rerun-with-compile
    process-history-rerun-with-async-shell-command)
  "Which commands to enable process history for.
This option works in union with `process-history-modes'."
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
  "%Y-%02m-%02d_%02H_%02M:%02S.log"
  "Log file format.
Warning: `process-history' won't find old logs if changed.
See `time-stamp-format'."
  :type 'string)

(defcustom process-history-annotate-alist
  '((process-history--annotate-directory 40)
    (process-history--annotate-active-time 20)
    (process-history--annotate-start-time 20)
    (process-history--item-condition 20)
    (process-history--item-vc 100))
  "Annotation formating specification.
Alist of (FUNCTION . LENGTH FACE) pairs."
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


;;; Global vars
(defvar process-history nil
  "History list of `process-history--item' items.")


;;; Data
(cl-defstruct (process-history--item (:type list))
  command start-time end-time exit-code directory vc condition process)


;;; Utils
(defun process-history--command-to-string (command)
  (string-join (if (equal (nth 1 command) shell-command-switch)
                   (nthcdr 2 command)
                 command)
               " "))

(defun process-history--log-file (item)
  (file-name-concat process-history-directory
                    (format-time-string process-history-log-format
                                        (process-history--item-start-time item))))

(defun process-history--prune ()
  (setq process-history
        (cl-loop with command-set = (make-hash-table :test 'equal)
                 for item in process-history
                 for command = (process-history--item-command item)
                 for unique-command-p = (not (gethash command command-set))
                 do (puthash command t command-set)
                 when (or (not process-history-prune-after)
                          (< (- (time-to-seconds)
                                (time-to-seconds
                                 (process-history--item-start-time item)))
                             process-history-prune-after)
                          (and process-history-prune-keep-unique
                               unique-command-p))
                 collect item)))


;;; Latch on `make-process'
(defun process-history--make-process (make-process &rest args)
  (let ((command
         (process-history--command-to-string (plist-get args :command)))
        (directory (or (plist-get args :directory) default-directory))
        (buffer (or (plist-get args :buffer) (current-buffer)))
        condition)
    (cond
     ((and
       ;; TODO Disabled for tramp as there is no world where it works
       (not (tramp-tramp-file-p directory))
       (not (string-empty-p (string-trim command)))
       (setq condition
             (car (or (memq (with-current-buffer buffer major-mode)
                            process-history-modes)
                      (memq this-command
                            process-history-this-command)))))
      (let ((item
             (make-process-history--item
              :command command
              :start-time (current-time)
              :directory directory
              :condition condition
              ;; FIXME Should be possible to support other vc backends
              :vc (vc-working-revision (file-name-as-directory directory) 'Git))))
        (plist-put args :filter
                   (process-history--make-filter item (plist-get args :filter)))
        (plist-put args :sentinel
                   (process-history--make-sentinel item (plist-get args :sentinel)))
        (push item process-history)
        (setf (process-history--item-process item) (apply make-process args))))
     (t
      (apply make-process args)))))

(defun process-history--set-process-filter (set-filter process filter)
  (if-let ((item (car (cl-member process
                                 process-history
                                 :key 'process-history--item-process))))
      (funcall set-filter process (process-history--make-filter item filter))
    (funcall set-filter process filter)))

(defun process-history--set-process-sentinel (set-sentinel process sentinel)
  (if-let ((item (car (cl-member process
                                 process-history
                                 :key 'process-history--item-process))))
      (funcall set-sentinel process
               (process-history--make-sentinel item sentinel))
    (funcall set-sentinel process sentinel)))

(defun process-history--make-filter (item filter)
  (let ((log-file (process-history--log-file item))
        (filter (or filter 'ignore)))
    (lambda (proc string)
      (unwind-protect
          (funcall filter proc string)
        (write-region string nil log-file 'append 'no-echo)))))

(defun process-history--make-sentinel (item sentinel)
  (let ((sentinel (or sentinel 'ignore)))
    (lambda (proc msg)
      (unwind-protect
          (funcall sentinel proc msg)
        (when (memq (process-status proc) '(exit signal))
          (setf (process-history--item-end-time item)
                (current-time)
                (process-history--item-exit-code item)
                (process-exit-status proc)))))))


;;; Annotation
(defun process-history--annotate-directory (item)
  (propertize
   (let ((directory (process-history--item-directory item)))
     (if (tramp-tramp-file-p directory)
         directory
       (abbreviate-file-name directory)))
   'face 'process-history-directory-face))

(defun process-history--annotate-active-time (item)
  (propertize
   (format-seconds process-history-seconds-format
                   (- (time-to-seconds
                       (process-history--item-end-time item))
                      (time-to-seconds (process-history--item-start-time item))))
   'face
   (cond
    ((and-let* ((process (process-history--item-process item)))
       (process-live-p process))
     'default)
    ((equal (process-history--item-exit-code item) 0)
     'process-history-success-face)
    (t 'process-history-error-face))))

(defun process-history--annotate-start-time (item)
  (format-time-string process-history-timestmap-format
                      (process-history--item-start-time item)))

(defun process-history--mode (item)
  (propertize (format "%s" (process-history--item-condition item))
              'face 'process-history-mode-face))

(defun process-history--annotate (item)
  (mapconcat (pcase-lambda (`(,fn ,length))
               (propertize (truncate-string-to-width
                            (format "%s" (or (funcall fn item) ""))
                            length 0 ?\s "...")))
             process-history-annotate-alist
             " "))


;;; Completion
(defun process-history--collection ()
  (cl-loop with command-count = (make-hash-table :test 'equal)
           for item in process-history
           for command = (process-history--item-command item)
           for count = (or (gethash command command-count) 0)
           do (puthash command (1+ count) command-count)
           collect (cons (concat command
                                 (unless (zerop count)
                                   (propertize (format "<%d>" count)
                                               'face 'shadow)))
                         item)))

(defun process-history--completing-read (prompt &optional predicate)
  (let* ((max-candidate-width 80)
         (alist (process-history--collection))
         (collection
          (lambda (string predicate action)
            (cond
             ((eq action 'metadata)
              `(metadata
                (category . process-history)
                (annotation-function
                 . ,(lambda (string)
                      (setq max-candidate-width
                            (max max-candidate-width (length string)))
                      (concat
                       (propertize " " 'display
                                   `(space :align-to
                                           (+ left ,max-candidate-width)))
                       " "
                       (process-history--annotate (alist-get string alist
                                                             nil nil 'equal)))))
                (display-sort-function . identity)))
             (t
              (complete-with-action action
                                    alist
                                    string
                                    predicate)))))
         ;; Properties are added in `process-history--collection'
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
  (process-history--prune)
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
                          (unless (numberp (process-history--item-exit-code item))
                            (setf (process-history--item-exit-code item) -1))
                          (unless (process-history--item-end-time item)
                            (setf (process-history--item-exit-code item) (current-time)))
                          ;; Drop `process' from item
                          (take (1- (cl-struct-slot-info 'process-history--item))
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
  (interactive (list (process-history--completing-read "Find process log or buffer: ")))
  (if-let* ((process (process-history--item-process history-item))
            ((processp process))
            (buffer (process-buffer process))
            ((buffer-live-p buffer))
            (buffer-process (or (get-buffer-process buffer)
                                process))
            ((eq process buffer-process)))
      (display-buffer buffer)
    (process-history-find-log history-item)))

(defun process-history-find-log (history-item)
  "View log file for HISTORY-ITEM."
  (interactive (list (process-history--completing-read "Find process log: ")))
  (find-file (process-history--log-file history-item))
  (process-history-log-mode))

(defun process-history-rerun-with-compile (history-item)
  "Rerun HISTORY-ITEM with `compile'."
  (interactive (list (process-history--completing-read "Rerun process with compile: ")))
  (let ((default-directory (process-history--item-directory history-item)))
    (compile (process-history--item-command history-item))))

(defun process-history-rerun-with-async-shell-command (history-item)
  "Rerun HISTORY-ITEM with `async-shell-command'."
  (interactive (list (process-history--completing-read
                      "Rerun with async-shell-command: ")))
  (let ((default-directory (process-history--item-directory history-item)))
    (async-shell-command (process-history--item-command history-item))))

(defun process-history-kill (history-item)
  "Kill active HISTORY-ITEM."
  (interactive
   (list (process-history--completing-read
          "Kill process: "
          (lambda (item)
            (when-let ((process (process-history--item-process item)))
              (and (processp process) (process-live-p process)))))))
  (kill-process (process-history--item-process history-item)))

(defun process-history-copy-as-kill-command (history-item)
  "Copy command of HISTORY-ITEM."
  (interactive
   (list (process-history--completing-read "Copy process command: ")))
  (kill-new (process-history--item-command history-item)))

(define-derived-mode process-history-log-mode special-mode "Log"
  "Mode active in `process-history' log files."
  :interactive nil
  (if-let ((item
            (cl-find-if (lambda (item)
                          (equal (process-history--log-file item)
                                 buffer-file-name))
                        process-history)))
      (setq buffer-read-only t
            header-line-format (concat " "
                                       (process-history--item-command item)
                                       "\t\t"
                                       (process-history--annotate item)))
    (user-error "Unable find connection with %s in `process-history'"
                buffer-file-name)))

;;;###autoload
(define-minor-mode process-history-mode
  "Extensive history for processes."
  :global t
  (cond
   (process-history-mode
    (advice-add 'make-process :around 'process-history--make-process)
    (advice-add 'set-process-filter :around 'process-history--set-process-filter)
    (advice-add 'set-process-sentinel :around 'process-history--set-process-sentinel)
    (add-hook 'kill-emacs-hook 'process-history-save)
    (when (and (file-exists-p process-history-save-file)
               (not process-history))
      (load process-history-save-file t)))
   (t
    (advice-remove 'make-process 'process-history--make-process)
    (advice-remove 'set-process-filter 'process-history--set-process-filter)
    (advice-remove 'set-process-sentinel 'process-history--set-process-sentinel)
    (remove-hook 'kill-emacs-hook 'process-history-save))))

(provide 'process-history)
;;; process-history.el ends here
