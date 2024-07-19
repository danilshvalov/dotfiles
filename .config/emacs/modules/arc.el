(defcustom arc-root "~/arcadia"
  "Path to mounted arcadia repository")

(defun arc--call-process (program &rest args)
  (require 'arc)
  (with-temp-buffer
    (let* ((default-directory (expand-file-name arc-root))
           (exit-code
            (apply 'call-process
                   program
                   nil
                   (current-buffer)
                   nil
                   args))
           (content (buffer-string)))
      (when (> exit-code 0)
        (error "Arc error: %s" content))
      content)))

(defun arc-make-link (with-line-number)
  (let ((file-name (buffer-file-name)))
    (unless (file-in-directory-p file-name arc-root)
      (error "File %s not in arcadia repository" file-name))
    (let* ((path (file-relative-name file-name arc-root))
           (link (format "https://a.yandex-team.ru/arcadia/%s%s"
                         path
                         (if with-line-number
                             (concat "#L" (number-to-string (1+ (count-lines 1 (point)))))
                           ""))))
      link)))

(defun arc-extract-ticket ()
  (let ((content (buffer-string))
        (case-fold-search nil))
    (when (string-match "\\([A-Z]+-[[:digit:]]+\\)" content)
      (match-string 1 content))))

(defun arc-find-file ()
  (interactive)
  (let ((path (read-file-name "Path: " (file-name-as-directory arc-root))))
    (find-file path)))

(defun arc-insert-ticket ()
  (interactive)
  (when-let* ((parser (treesit-parser-create 'gitcommit))
              (root (treesit-parser-root-node parser))
              (query (treesit-query-compile 'gitcommit
                                            '((comment) @capture)))
              (ticket (arc-extract-ticket))
              (captures (treesit-query-capture root query))
              (capture (alist-get 'capture captures)))
    (save-excursion
      (goto-char (treesit-node-start capture))
      (insert (format "\nRelates: %s\n" ticket)))))

(defun arc--merge-base-trunk ()
  (string-trim
   (arc--call-process "arc" "merge-base" "HEAD" "trunk")))

(defun arc--diff-trunk ()
    (arc--call-process "arc" "diff" (arc--merge-base-trunk) "HEAD" "--no-color"))

(defun arc-diff-trunk ()
  (interactive)
  (let* ((diff-content (arc--diff-trunk))
         (buffer (get-buffer-create "arc-diff-trunk")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert diff-content)
      (goto-char (point-min))
      (diff-ts-mode)
      (read-only-mode)
      (set-window-buffer nil buffer))))

(defun arc--branches-raw ()
  (let* ((branches (arc--call-process "arc" "branch" "--json"))
         (branches (json-parse-string branches)))
    branches))

(defun arc--branch-current ()
  (let* ((branches (arc--branches-raw))
         (branches (cl-remove-if-not (lambda (x) (gethash "current" x nil)) branches))
         (branch (aref branches 0))
         (branch (gethash "name" branch)))
    branch))

(defun arc--branches ()
  (let* ((branches (arc--branches-raw))
         (branches (cl-remove-if-not (lambda (x) (gethash "local" x)) branches))
         (branches (mapcar (lambda (x) (gethash "name" x)) branches)))
    branches))

(defun arc--select-branch (&optional prompt)
  (let* ((prompt (or prompt "Select branch: "))
         (branches (arc--branches))
         (branch (completing-read prompt branches nil t)))
    branch))

(defun arc--select-branch-list ()
  (let* ((branches (arc--branches))
         (branch (completing-read-multiple "Select branch: " branches nil t)))
    branch))

(defun arc-branch-checkout (&optional branch)
  (interactive)
  (let* ((prompt (format "Select branch (current: %s): " (arc--branch-current)))
         (branch (or branch (arc--select-branch prompt))))
    (arc--call-process "arc" "co" branch)))

(defun arc-pull ()
  (interactive)
  (arc--call-process "arc" "pull"))

(defun arc-info ()
  (interactive)
  (message "Arc: %s" (arc--branch-current)))

(defun arc-branch-delete (&optional branches)
  (interactive)
  (let* ((branches (or branches (arc--select-branch-list))))
    (apply 'arc--call-process "arc" "branch" "--delete" branches)))

(provide 'arc)
