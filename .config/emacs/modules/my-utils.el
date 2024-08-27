;; -*-lexical-binding: t -*-

(defmacro use-builtin (name &rest args)
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

(defun get-pwd ()
  (let ((client (frame-parameter nil 'client)))
    (if client
        (process-get client 'server-client-directory)
      (getenv "PWD"))))

(defvar my-default-directory
  (file-name-as-directory (or (get-pwd) "~")))

(defvar my-directories nil)

(defun my-get-current-directory ()
  (or (assoc-default (my-tab-name-current) my-directories)
      (or (get-pwd) "~")))

(defun my-set-current-directory (directory)
  (setq my-directories (push (cons (my-tab-name-current) directory) my-directories)))

(defun project-root-current ()
  (my-get-current-directory))

(defun add-to-list! (list &rest args)
  (dolist (item args)
    (add-to-list list item)))

(defun add-hook! (hook function &optional depth local)
  (let ((hook (if (nlistp hook) (list hook) hook)))
    (dolist (item hook)
      (add-hook item function depth local))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message
   (concat
    (propertize "Current file:" 'face 'bold)
    " "
    (abbreviate-file-name (buffer-file-name)))))

(defun show-datetime ()
  (interactive)
  (message
   (concat
    (propertize "Current datetime:" 'face 'bold)
    " "
    (format-time-string "%A %d.%m %H:%M"))))

(defun execute-at-project-root (orig-fun &rest args)
  (let ((default-directory (project-root-current)))
    (apply orig-fun args)))

(defun inhibit-sentinel-messages (fun &rest args)
  "Inhibit messages in all sentinels started by fun."
  (cl-letf*
      ((old-set-process-sentinel (symbol-function 'set-process-sentinel))
       ((symbol-function 'set-process-sentinel)
        (lambda (process sentinel)
          (funcall old-set-process-sentinel
                   process
                   `(lambda (&rest args)
                      (let ((inhibit-message t))
                        (apply (quote ,sentinel) args)))))))
    (apply fun args)))

(defun new-instance--darwin ()
  (call-process-shell-command "open -na Emacs"))

(defun new-instance ()
  (interactive)
  (pcase system-type
    ('darwin (new-instance--darwin))
    (type (error "New instance isn't implemented for \"%s\"" type))))

(defun open-finder (&optional path)
  (interactive "P")
  (let* ((path (or path "."))
         (path
          (cond
           ((listp path) (string-join path " "))
           (t path)))
       (command (list "open" path)))
    (call-process-shell-command (string-join command " "))))

(defun tab-move-previous ()
  (interactive)
  (tab-move -1))

(defun tab-move-next ()
  (interactive)
  (tab-move 1))

(provide 'my-utils)
