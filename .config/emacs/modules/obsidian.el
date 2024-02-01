;; -*-lexical-binding: t -*-

(defgroup obsidian nil
  "Obsidian integration for Emacs."
  :link '(url-link :tag "Homepage" "https://github.com/danilshvalov/obsidian")
  :link '(emacs-library-link :tag "Library Source" "obsidian.el")
  :prefix "obsidian-")

(defcustom obsidian-workspaces nil
  ""
  :type 'alist)

(defcustom obsidian-daily-note-directory nil
  ""
  :type 'string)

(defvar obsidian--current-workspace nil)

(defvar obsidian--rg-proc)

(defvar obsidian--rg-output-buffer "*obsidian-rg*")

(defun obsidian--check-workspaces ()
  (unless obsidian-workspaces
    (error "Workspaces are not defined")))

(defun obsidian-current-workspace ()
  (obsidian--check-workspaces)
  (or obsidian--current-workspace (car obsidian-workspaces)))

(defun obsidian-current-root ()
  (cdr (obsidian-current-workspace)))

(defun obsidian--select-workspace ()
  (completing-read "Select workspace: " obsidian-workspaces nil t))

(defun obsidian-switch-workspace (&optional workspace-name)
  (interactive)
  (obsidian--check-workspaces)

  (when (and workspace-name (not (assoc workspace-name obsidian-workspaces)))
    (error "Workspace %s is not defined"))

  (let* ((workspace-name (or workspace-name (obsidian--select-workspace))))
    (setq obsidian--current-workspace (assoc (intern workspace-name) obsidian-workspaces))))

(defun obsidian-daily-note (&optional day-offset)
  (interactive)
  (let* ((day-offset (or day-offset (read-number "Enter day offset: " 0)))
         (obsidian-root (obsidian-current-root))
         (daily-note-root (file-name-concat obsidian-root obsidian-daily-note-directory))
         (template-path "~/obsidian/шаблоны/Ежедневник.md")
         (note-date (time-add (current-time) (days-to-time day-offset)))
         (date-format "%F")
         (note-format "%Y/%m/%d.md")
         (note-path (format-time-string
                     (file-name-concat daily-note-root note-format)
                     note-date)))
    (find-file note-path)
    (make-directory (file-name-directory note-path) t)

    (when (string-empty-p (string-trim (buffer-string)))
      (insert-file-contents template-path)
      (replace-string "{{date}}" (format-time-string date-format note-date)))

    (write-file note-path)))

(defun obsidian-today ()
  (interactive)
  (obsidian-daily-note 0))

(defun obsidian-yesterday ()
  (interactive)
  (obsidian-daily-note -1))

(defun obsidian-tomorrow ()
  (interactive)
  (obsidian-daily-note +1))

(defun obsidian-open (&optional filename)
  (interactive)
  (let* ((obsidian-root "~/obsidian")
         (filename (file-relative-name (or filename (buffer-file-name)) obsidian-root))
         (vault-name (file-name-base (file-truename obsidian-root))))
    (shell-command (format "open -a /Applications/Obsidian.app 'obsidian://open?vault=%s&file=%s'" vault-name filename))))

(defun obsidian-new ()
  (interactive)
  (let* ((obsidian-root "~/obsidian")
         (filename (read-string "Enter filename: " "заметки/"))
         (filename (downcase filename))
         (filename (replace-regexp-in-string "[[:space:]]+" "-" filename))
         (filename (replace-regexp-in-string "[^[:alnum:]-/]" "" filename))
         (filename (file-name-with-extension filename "md"))
         (note-path (file-name-concat obsidian-root filename)))
    (find-file note-path)
    (make-directory (file-name-directory note-path) t)
    (write-file note-path)))

(defun obsidian--insert-tag (source-buffer output)
  (let* ((tags (delete-dups (string-split output "\n" t "\s+")))
         (tags (sort tags 'string-lessp))
         (tag (completing-read "Select tag: " tags)))
    (with-current-buffer source-buffer
      (insert "#" tag))))

(defun obsidian-insert-tag ()
  (interactive)
  (obsidian--search-tags-async (current-buffer) 'obsidian--insert-tag))

(defun obsidian--search-tags-async (source-buffer fn)
    (setq
     obsidian--rg-proc
     (make-process
      :name "obsidian-rg"
      :buffer (generate-new-buffer obsidian--rg-output-buffer)
      :command `("rg"
                 "--no-config"
                 "--color=never"
                 "--type=md"
                 "--ignore-case"
                 "-o"
                 "-I"
                 "-N"
                 "-r $1"
                 "#([\\w\\d_/-]+)"
                 ,(file-truename (obsidian-current-root)))
      :sentinel
      (lambda (proc _event)
        (interactive)
        (when (eq 'exit (process-status proc))
          (unwind-protect
              (when (with-current-buffer source-buffer (eq proc obsidian--rg-proc))
                (with-current-buffer (process-buffer proc)
                  (let ((output (buffer-string)))
                    (funcall fn source-buffer output))))
            (with-current-buffer source-buffer
              (kill-buffer (process-buffer obsidian--rg-proc))
              (setq obsidian--rg-proc nil))))))))

(provide 'obsidian)
