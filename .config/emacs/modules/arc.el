(defcustom arc-root "~/arcadia"
  "Path to mounted arcadia repository")

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

(provide 'arc)
