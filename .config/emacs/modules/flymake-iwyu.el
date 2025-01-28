;;; flymake-iwyu.el --- flymake integration for iwyu -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Daniil Shvalov
;;
;; Maintainer: Daniil Shvalov <http://github.com/danilshvalov>
;; URL: https://github.com/danilshvalov/flymake-iwyu
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (flymake "0.22") (let-alist "1.0.4"))
;; Keywords: flymake, iwyu
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This provides flymake integration for iwyu
;;
;; Usage example:
;;   (require 'flymake-iwyu)
;;   (add-hook 'c++-ts-mode-hook #'flymake-iwyu-load)
;;
;;; Code:

(require 'flymake)
(require 'let-alist)
(require 'json)

(defgroup flymake-iwyu nil
  "Variables related to flymake-iwyu."
  :prefix "flymake-iwyu-"
  :group 'tools)

(defcustom flymake-iwyu-program (executable-find "iwyu-tool")
  "The Iwyu executable to use."
  :type 'string
  :group 'flymake-iwyu)

(defcustom flymake-iwyu-output-buffer "*flymake-iwyu*"
  "Buffer where tool output gets written."
  :type 'string
  :group 'flymake-iwyu)

(defvar-local flymake-iwyu--proc nil
  "A buffer-local variable handling the iwyu process for flymake.")

(defun flymake-iwyu-compile-commands-path ()
  (file-name-concat (project-root (project-current))
                    "compile_commands.json"))

(defun flymake-iwyu--check-all (source-buffer errors)
  "Parse ERRORS into flymake error structs."
  (let (check-list)
    (dolist (error errors)
      (let-alist error
        (let ((start-pos (flymake-diag-region source-buffer .line .start_column))
              (end-pos (flymake-diag-region source-buffer .line .end_column)))
          (push (flymake-make-diagnostic
                 source-buffer
                 (car start-pos)
                 (cdr end-pos)
                 :warning
                 .message)
                check-list))))
    check-list))

(defun flymake-iwyu--output-to-errors (report-fn)
  (goto-char (point-min))
  (cl-loop
   while (search-forward-regexp
          "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): warning: \\(.*\\)$"
          nil t)
   for source-buffer = (get-file-buffer (match-string 1))
   for msg = (match-string 4)
   for (beg . end) = (flymake-diag-region
                      source-buffer
                      (string-to-number (match-string 2)))
   for type = :warning
   when (and beg end)
   collect (flymake-make-diagnostic source-buffer
                                    beg
                                    end
                                    type
                                    msg)
   into diags
   finally (funcall report-fn diags)))

(defun flymake-iwyu--start (source-buffer report-fn)
  "Run iwyu on the current buffer's contents."
  ;; kill and cleanup any ongoing processes. This is meant to be more
  ;; performant instead of checking when the iwyu process finishes.
  (when (process-live-p flymake-iwyu--proc)
    (flymake-log :warning "Canceling the obsolete check %s"
                 (process-buffer flymake-iwyu--proc))
    (kill-buffer (process-buffer flymake-iwyu--proc))
    (delete-process flymake-iwyu--proc)
    (setq flymake-iwyu--proc nil))
  (setq
   flymake-iwyu--proc
   (make-process
    :name "flymake-iwyu-process"
    :noquery t
    :connection-type 'pipe
    :buffer (generate-new-buffer flymake-iwyu-output-buffer)
    :command `(,flymake-iwyu-program
               "-p" ,(flymake-iwyu-compile-commands-path)
               "-o" "clang-warning"
               ,(buffer-file-name source-buffer))
    :sentinel
    (lambda (proc _event)
      (when (eq 'exit (process-status proc))
        (unwind-protect
            (if (with-current-buffer source-buffer (eq proc flymake-iwyu--proc))
                (with-current-buffer (process-buffer proc)
                  (flymake-iwyu--output-to-errors report-fn))
              (with-current-buffer source-buffer
                (flymake-log :warning "Canceling obsolete check %s"
                             (process-buffer proc))))
          (with-current-buffer source-buffer
            (kill-buffer (process-buffer flymake-iwyu--proc))
            (setq flymake-iwyu--proc nil)))))))
  (process-send-region flymake-iwyu--proc (point-min) (point-max))
  (process-send-eof flymake-iwyu--proc))

(defun flymake-iwyu--checker (report-fn &rest _args)
  "Diagnostic checker function with REPORT-FN."
  (flymake-iwyu--start (current-buffer) report-fn))

;;;###autoload
(defun flymake-iwyu-load ()
  "Convenience function to setup flymake-iwyu.
This adds the iwyu checker to the list of flymake diagnostic
functions."
  (add-hook
   'flymake-diagnostic-functions
   #'flymake-iwyu--checker nil t))

(provide 'flymake-iwyu)
;;; flymake-iwyu.el ends here
