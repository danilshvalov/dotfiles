;;; flymake-sqlfluff.el --- flymake integration for sqlfluff -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniil Shvalov
;;
;; Maintainer: Daniil Shvalov <http://github.com/danilshvalov>
;; URL: https://github.com/danilshvalov/flymake-sqlfluff
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (flymake "0.22") (let-alist "1.0.4"))
;; Keywords: flymake, sqlfluff
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
;; This provides flymake integration for sqlfluff
;;
;; Usage example:
;;   (require 'flymake-sqlfluff)
;;   (add-hook 'sql-mode-hook #'flymake-sqlfluff-load)
;;
;;; Code:

(require 'flymake)
(require 'let-alist)
(require 'json)

(defgroup flymake-sqlfluff nil
  "Variables related to flymake-sqlfluff."
  :prefix "flymake-sqlfluff-"
  :group 'tools)

(defcustom flymake-sqlfluff-program (executable-find "sqlfluff")
  "The Sqlfluff executable to use."
  :type 'string
  :group 'flymake-sqlfluff)

(defvar flymake-sqlfluff--dialect-options '("ansi"
                                            "athena"
                                            "bigquery"
                                            "clickhouse"
                                            "databricks"
                                            "db2"
                                            "exasol"
                                            "hive"
                                            "mysql"
                                            "oracle"
                                            "postgres"
                                            "redshift"
                                            "snowflake"
                                            "soql"
                                            "sparksql"
                                            "sqlite"
                                            "teradata"
                                            "tsql")
  "List of supported dialects.")

(defcustom flymake-sqlfluff-dialect "postgres"
  "List of possible dialect to be checked."
  :group 'flymake-sqlfluff
  :options flymake-sqlfluff--dialect-options
  :type 'list)

;;;###autoload
(defun flymake-sqlfluff-change-dialect ()
  "Change sqlfluff dialect."
  (interactive)
  (setq flymake-sqlfluff-dialect
        (completing-read
         "Choose dialect: "
         flymake-sqlfluff--dialect-options)))

(defcustom flymake-sqlfluff-output-buffer "*flymake-sqlfluff*"
  "Buffer where tool output gets written."
  :type 'string
  :group 'flymake-sqlfluff)

(defvar-local flymake-sqlfluff--proc nil
  "A buffer-local variable handling the sqlfluff process for flymake.")

(defun flymake-sqlfluff--check-all (source-buffer errors)
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

(defun flymake-sqlfluff--output-to-errors (source-buffer output)
  "Parse the full JSON OUTPUT of sqlfluff.
Converts output into a sequence of flymake error structs."
  (let* ((json-array-type 'list)
         (errors (json-read-from-string output)))
    (with-current-buffer source-buffer
      (flymake-sqlfluff--check-all source-buffer errors))))

(defun flymake-sqlfluff--start (source-buffer report-fn)
  "Run sqlfluff on the current buffer's contents."
  ;; kill and cleanup any ongoing processes. This is meant to be more
  ;; performant instead of checking when the sqlfluff process finishes.
  (when (process-live-p flymake-sqlfluff--proc)
    (flymake-log :warning "Canceling the obsolete check %s"
                 (process-buffer flymake-sqlfluff--proc))
    (kill-buffer (process-buffer flymake-sqlfluff--proc))
    (delete-process flymake-sqlfluff--proc)
    (setq flymake-sqlfluff--proc nil))
  (setq
   flymake-sqlfluff--proc
   (make-process
    :name "flymake-sqlfluff-process"
    :noquery t
    :connection-type 'pipe
    :buffer (generate-new-buffer flymake-sqlfluff-output-buffer)
    :command `(,flymake-sqlfluff-program
               "lint"
               "--dialect" ,flymake-sqlfluff-dialect
               "--format" "github-annotation"
               "--disable-progress-bar"
               "-")
    :sentinel
    (lambda (proc _event)
      (when (eq 'exit (process-status proc))
        (unwind-protect
            (if (with-current-buffer source-buffer (eq proc flymake-sqlfluff--proc))
                (with-current-buffer (process-buffer proc)
                  (let ((output (buffer-string)))
                    (funcall report-fn (flymake-sqlfluff--output-to-errors
                                        source-buffer output))))
              (with-current-buffer source-buffer
                (flymake-log :warning "Canceling obsolete check %s"
                             (process-buffer proc))))
          (with-current-buffer source-buffer
            (kill-buffer (process-buffer flymake-sqlfluff--proc))
            (setq flymake-sqlfluff--proc nil)))))))
  (process-send-region flymake-sqlfluff--proc (point-min) (point-max))
  (process-send-eof flymake-sqlfluff--proc))

(defun flymake-sqlfluff--checker (report-fn &rest _args)
  "Diagnostic checker function with REPORT-FN."
  (flymake-sqlfluff--start (current-buffer) report-fn))

;;;###autoload
(defun flymake-sqlfluff-load ()
  "Convenience function to setup flymake-sqlfluff.
This adds the sqlfluff checker to the list of flymake diagnostic
functions."
  (add-hook
   'flymake-diagnostic-functions
   #'flymake-sqlfluff--checker nil t))

(provide 'flymake-sqlfluff)
;;; flymake-sqlfluff.el ends here
