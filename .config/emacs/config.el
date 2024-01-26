;; -*-lexical-binding: t -*-

(defmacro use-builtin (name &rest args)
  (declare (indent defun))
  `(use-package ,name
     :elpaca nil
     ,@args))

(defun add-to-list! (list &rest args)
  (dolist (item args)
    (add-to-list list item)))

(defun add-hook! (hook function &optional depth local)
  (let ((hook (if (nlistp hook) (list hook) hook)))
    (dolist (item hook)
      (add-hook item function depth local))))

(use-package general
  :demand t
  :config
  (general-auto-unbind-keys t)
  (general-evil-setup t))

(elpaca-wait)

(use-package emacs
  :elpaca nil
  :custom
  (confirm-kill-emacs 'y-or-n-p)
  (scroll-margin 10)
  (hscroll-margin 20)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (ring-bell-function 'ignore)
  (fill-column 80)
  (use-short-answers t)
  (ns-right-option-modifier nil)
  (make-backup-files nil)
  (auto-save-default nil)
  (whitespace-style '(face tabs))
  (window-combination-resize t)
  (split-width-threshold t)
  (help-window-select t)
  (bidi-display-reordering nil)
  (sentence-end-double-space nil)
  (history-length t)
  (visible-cursor nil)
  (warning-minimum-level :emergency)
  :hook
  (before-save . delete-trailing-whitespace)
  ((prog-mode text-mode) . display-fill-column-indicator-mode)
  :preface
  (defvar my-default-directory
    (file-name-as-directory (or (getenv "PWD") "~")))

  (defvar my-directories nil)
  (defvar my-directory nil)

  ;; (defun my-get-current-directory ()
  ;;   (or my-directory (or (getenv "PWD") "~"))
  ;;   ;; (or (assoc-default (my-tab-name-current) my-directories)
  ;;   ;;     (or (getenv "PWD") "~"))
  ;;   )

  ;; (defun my-set-current-directory (directory)
  ;;   (setq my-directory directory)
  ;;   ;; (add-to-list 'my-directories (cons (my-tab-name-current) directory))
  ;;   )

  ;; remove image resize delay
  (advice-add 'image--delayed-change-size :override 'image--change-size)

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
        (
         (old-set-process-sentinel (symbol-function 'set-process-sentinel))
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
    (let*
        ((path (or path "."))
         (path
          (cond
           ((listp path)
            (string-join path " "))
           (t
            path)))
         (command (list "open" path)))
      (call-process-shell-command (string-join command " "))))

  :general
  (nvmap
    :prefix "C-x"
    "j" 'next-buffer
    "k" 'previous-buffer)

  (nvmap
    :keymaps 'override
    "H" "^"
    "L" "$")

  (:keymaps 'override
            "C-s-f" 'toggle-frame-fullscreen)

  (nmap
    :keymaps 'override
    "ZX" 'kill-current-buffer)

  (nvmap
    :prefix "SPC d"
    "y" (lambda ()
          (interactive)
          (let ((path (project-root-current)))
            (kill-new path)
            (message "Path \"%s\" is copied to the clipboard" path))))

  (nvmap :prefix "SPC s" "f" 'show-file-name "d" 'show-datetime)

  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons
     (format "[CRM%s] %s"
             (replace-regexp-in-string
              "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'"
              ""
              crm-separator)
             (car args))
     (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  :config
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq buffer-file-coding-system 'utf-8-unix)

  (menu-bar-mode -1)
  (global-whitespace-mode +1)
  (window-divider-mode)
  (savehist-mode)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(use-package doom-themes
  :config
  (load-theme 'doom-tokyonight-storm t)

  (doom-themes-set-faces 'user
    `(highlight
      :foreground 'unspecified
      :background 'unspecified
      :inherit 'region)
    `(font-lock-comment-face :foreground ,(doom-lighten 'comments 0.2))
    `(corfu-current :background ,(doom-color 'region))
    `(evil-snipe-matches-face
      :foreground 'unspecified
      :background ,(doom-color 'region)
      :underline nil)
    `(highlight-doxygen-comment :background 'unspecified)
    `(highlight-doxygen-code-block :background 'unspecified)
    `(markdown-code-face :background 'unspecified)
    `(markdown-list-face :foreground ,(doom-color 'yellow))
    `(font-lock-operator-face :foreground ,(doom-color 'operators))
    `(font-lock-punctuation-face :foreground ,(doom-color 'punctuations))
    `(tab-bar-tab :background ,(doom-color 'region))
    `(flymake-end-of-line-diagnostics-face :height 'unspecified :box 'unspecified))

  (add-hook 'image-mode-hook
            (lambda ()
              (face-remap-add-relative 'default '(:background "white"))
              (face-remap-add-relative 'cursor '(:background "white")))))

(defun concat-lines (&rest args)
  (string-join args "\n"))

(use-package project
  :requires consult
  :preface
  (defun dired-project-root ()
    (interactive)
    (let ((default-directory (project-root-current)))
      (dired default-directory)))
  :custom
  (project-switch-commands '((consult-fd "Find file")
                             (dired-project-root "Dired")))
  :general
  (:keymaps 'project-prefix-map
            "f" 'consult-fd
            "d" 'dired-project-root)
  (nmap
    :prefix "SPC p"
    "s" 'project-switch-project
    "a" 'my-project-add)

  :preface
  ;; (cl-defmethod project-root ((project (head local)))
  ;;   "Return root directory of current PROJECT."
  ;;   (cdr project))

  (defun project-root-current ()
    (let ((current (project-current)))
      (if current
          (project-root current)
        default-directory))
    ;; (or project-current-directory-override
    ;;     default-directory
    ;;     ; (my-get-current-directory)
    ;;     ;; my-default-directory
    ;;     )
    )

  ;; (defun project-find-root (dir)
;;     "Determine if DIR is a non-VC project.
;; DIR must include a .project file to be considered a project."
;;     (cons 'local (project-root-current)))

  (defun my-project-add (&optional dir)
    (interactive)
    (let ((dir (or dir
                   (read-directory-name "Remember project at: "
                                        default-directory
                                        default-directory
                                        t))))
      (project-remember-project (cons 'local dir))))
  :init

  ;; (add-hook 'project-find-functions #'project-find-root)
  )

(use-package project-x
  :demand t
  :elpaca (:host github :repo "karthink/project-x")
  :after project
  :custom
  (project-x-local-identifier '(".project" "a.yaml"))
  :config
  (add-hook 'project-find-functions 'project-x-try-local 90)
  ;; (add-hook 'kill-emacs-hook 'project-x--window-state-write)
  ;; (add-to-list 'project-switch-commands
  ;;              '(?j "Restore windows" project-x-windows) t)
  :bind (("C-x p w" . project-x-window-state-save)
         ("C-x p j" . project-x-window-state-load)))

(set-frame-font "JetBrains Mono 17" nil t)

(custom-set-faces
 `(default ((t (:font "JetBrains Mono 17"))))
 `(fixed-pitch ((t (:inherit (default)))))
 `(fixed-pitch-serif ((t (:inherit (default)))))
 `(variable-pitch ((t (:inherit (default)))))
 `(variable-pitch-text ((t (:height 1.0)))))

(add-hook 'c++-ts-mode-hook (lambda () (setq c-ts-mode-indent-offset 2)))

(defun c++-ts-mode-indent-style ()
  `((c-ts-mode--for-each-tail-body-matcher prev-line c-ts-mode-indent-offset)

    ((parent-is "argument_list") parent-bol c-ts-mode-indent-offset)
    ((parent-is "parameter_list") parent-bol c-ts-mode-indent-offset)
    ((parent-is "translation_unit") column-0 0)
    ((query "(ERROR (ERROR)) @indent") column-0 0)
    ((node-is ")") parent 1)
    ((node-is "]") parent-bol 0)
    ((node-is "else") parent-bol 0)
    ((node-is "case") parent-bol 0)
    ((node-is "preproc_arg") no-indent)
    ;; `c-ts-common-looking-at-star' has to come before
    ;; `c-ts-common-comment-2nd-line-matcher'.
    ((and (parent-is "comment") c-ts-common-looking-at-star)
     c-ts-common-comment-start-after-first-star
     -1)
    (c-ts-common-comment-2nd-line-matcher c-ts-common-comment-2nd-line-anchor 1)
    ((parent-is "comment") prev-adaptive-prefix 0)

    ;; Labels.
    ((node-is "labeled_statement") standalone-parent 0)
    ((parent-is "labeled_statement")
     c-ts-mode--standalone-grandparent
     c-ts-mode-indent-offset)

    ;; Preproc directives
    ((node-is "preproc") column-0 0)
    ((node-is "#endif") column-0 0)
    ((match "preproc_call" "compound_statement") column-0 0)

    ;; Top-level things under a preproc directive.  Note that
    ;; "preproc" matches more than one type: it matches
    ;; preproc_if, preproc_elif, etc.
    ((n-p-gp nil "preproc" "translation_unit") column-0 0)
    ;; Indent rule for an empty line after a preproc directive.
    ((and no-node (parent-is ,(rx (or "\n" "preproc"))))
     c-ts-mode--standalone-parent-skip-preproc
     c-ts-mode--preproc-offset)
    ;; Statement under a preproc directive, the first statement
    ;; indents against parent, the rest statements indent to
    ;; their prev-sibling.
    ((match nil ,(rx "preproc_" (or "if" "elif")) nil 3 3)
     c-ts-mode--standalone-parent-skip-preproc
     c-ts-mode-indent-offset)
    ((match nil "preproc_ifdef" nil 2 2)
     c-ts-mode--standalone-parent-skip-preproc
     c-ts-mode-indent-offset)
    ((match nil "preproc_else" nil 1 1)
     c-ts-mode--standalone-parent-skip-preproc
     c-ts-mode-indent-offset)
    ((parent-is "preproc") c-ts-mode--anchor-prev-sibling 0)

    ((parent-is "function_definition") parent-bol 0)
    ((parent-is "conditional_expression") first-sibling 0)
    ((parent-is "assignment_expression") parent-bol c-ts-mode-indent-offset)
    ((parent-is "concatenated_string") first-sibling 0)
    ((parent-is "comma_expression") first-sibling 0)
    ((parent-is "init_declarator") parent-bol c-ts-mode-indent-offset)
    ((parent-is "parenthesized_expression") parent-bol c-ts-mode-indent-offset)
    ((parent-is "binary_expression") parent 0)
    ((query "(for_statement initializer: (_) @indent)") parent-bol 5)
    ((query "(for_statement condition: (_) @indent)") parent-bol 5)
    ((query "(for_statement update: (_) @indent)") parent-bol 5)
    ((query "(call_expression arguments: (_) @indent)")
     parent
     c-ts-mode-indent-offset)
    ((parent-is "call_expression") parent 0)
    ;; Closing bracket.  This should be before initializer_list
    ;; (and probably others) rule because that rule (and other
    ;; similar rules) will match the closing bracket.  (Bug#61398)
    ((node-is "}") standalone-parent 0)
    ((node-is "access_specifier") parent-bol 3)

    ((parent-is "initializer_list") parent-bol c-ts-mode-indent-offset)
    ((parent-is "enumerator_list") parent-bol c-ts-mode-indent-offset)
    ((parent-is "field_declaration_list") parent-bol c-ts-mode-indent-offset)
    ((parent-is "declaration_list") parent-bol 0)

    ;; Statement in {} blocks.
    ((or (match nil "compound_statement" nil 1 1)
         (match null "compound_statement"))
     standalone-parent c-ts-mode-indent-offset)
    ((parent-is "compound_statement") c-ts-mode--anchor-prev-sibling 0)
    ;; Opening bracket.
    ((node-is "compound_statement") standalone-parent c-ts-mode-indent-offset)
    ;; Bug#61291.
    ((match "expression_statement" nil "body")
     standalone-parent
     c-ts-mode-indent-offset)
    ;; These rules are for cases where the body is bracketless.
    ;; Tested by the "Bracketless Simple Statement" test.
    ((parent-is "if_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "for_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "while_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "do_statement") standalone-parent c-ts-mode-indent-offset)

    ((parent-is "case_statement") standalone-parent c-ts-mode-indent-offset)

    ((node-is "field_initializer_list")
     parent-bol
     ,(* c-ts-mode-indent-offset 2))))

(setq c-ts-mode-indent-style 'c++-ts-mode-indent-style)

(use-package highlight-doxygen
  :hook (c++-ts-mode . highlight-doxygen-mode))

(use-package adaptive-wrap
  :hook
  ((prog-mode text-mode vterm-mode) . visual-line-mode)
  (visual-line-mode . +adaptive-wrap-prefix-mode)
  (calendar-mode . +disable-visual-line-mode)
  :preface
  (defun +adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (unless
        (or (equal major-mode 'org-mode) (equal major-mode 'org-journal-mode))
      (adaptive-wrap-prefix-mode
       (if visual-line-mode 1 -1))))

  (defun +disable-visual-line-mode ()
    (visual-line-mode -1)
    (setq-local truncate-lines t))

  (defun toggle-wrap ()
    (interactive)
    (let ((inhibit-message t))
      (if visual-line-mode
          (progn
            (visual-line-mode -1)
            (toggle-truncate-lines 1))
        (progn
          (toggle-truncate-lines -1)
          (visual-line-mode 1)))))
  :general
  (nvmap
    :prefix "SPC t"
    "w" 'toggle-wrap
    "r" 'auto-fill-mode))

(use-package hl-todo
  :hook
  ((prog-mode text-mode) . hl-todo-mode)
  (org-mode . (lambda () (hl-todo-mode -1)))
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces '(("TODO" warning bold)
                           ("FIXME" error bold)
                           ("REVIEW" font-lock-keyword-face bold)
                           ("HACK" font-lock-constant-face bold)
                           ("DEPRECATED" error bold)
                           ("WARN" warning bold)
                           ("WARNING" warning bold)
                           ("NOTE" warning bold)
                           ("BUG" error bold)
                           ("XXX" font-lock-constant-face bold))))

(use-builtin electric
  :config
  (electric-indent-mode +1)
  (electric-pair-mode t))

(use-package yasnippet
  :hook
  ((text-mode prog-mode) . yas-minor-mode)
  (org-mode . (lambda () (yas-activate-extra-mode 'latex-mode)))
  :config
  (yas-reload-all)
  (setq yas-indent-line 'fixed)
  (setq yas-triggers-in-field t))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(use-package avy
  :custom
  (avy-all-windows nil)
  :custom-face
  (avy-lead-face   ((t (:background ,(doom-color 'yellow)))))
  (avy-lead-face-0 ((t (:background ,(doom-lighten 'yellow 0.40)))))
  (avy-lead-face-1 ((t (:background ,(doom-lighten 'yellow 0.60)))))
  (avy-lead-face-2 ((t (:background ,(doom-lighten 'yellow 0.80)))))
  :general
  (nvmap "s" 'avy-goto-char-2))

(use-builtin eglot
  :custom
  (eglot-sync-connect nil)
  (eglot-events-buffer-size 0)
  :hook
  ((LaTeX-mode
    c++-ts-mode
    csharp-mode
    python-ts-mode
    conf-toml-mode
    tsx-ts-mode)
   .
   eglot-ensure)
  :general
  (nmap
    :prefix "SPC c"
    :keymaps
    'override
    "r"
    'eglot-rename
    "a"
    'eglot-code-actions)
  :config
  (fset #'jsonrpc--log-event #'ignore)

  (fset #'eglot--snippet-expansion-fn #'ignore)

  (add-to-list 'eglot-server-programs '(c++-ts-mode . ("clangd" "--compile-commands-dir=build_debug")))
  (add-to-list 'eglot-server-programs '(latex-mode . ("texlab")))
  ;; (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp")))
  (add-to-list
   'eglot-server-programs
   '(conf-toml-mode . ("taplo" "lsp" "stdio"))))

(use-package vertico
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle t)
  :general
  (imap
    :keymaps 'vertico-map
    "C-j" 'vertico-next
    "C-k" 'vertico-previous)
  :init
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply
           (if vertico-mode
               #'consult-completion-in-region
             #'completion--in-region)
           args)))
  (vertico-mode))

(use-package consult
  :custom
  (consult-buffer-filter
   '("\\` "
     "\\`\\*Completions\\*\\'"
     "\\`\\*Messages\\*\\'"
     "\\`\\*Help\\*\\'"
     "\\`\\*Flymake log\\*\\'"
     "\\`\\*Semantic SymRef\\*\\'"
     "\\`\\*WoMan-Log\\*\\'"
     "\\`\\*Async-native-compile-log\\*\\'"
     "\\`\\*tramp/.*\\*\\'"
     "\\`\\*Eglot .*\\*\\'"))
  (consult-ripgrep-args
   "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip")
  (consult-async-split-style 'semicolon)
  :general
  (nvmap
    :keymaps 'override
    :prefix "SPC f"
    "" '(nil :wk "find")
    "r" '+consult-recent-file
    "b" 'consult-buffer
    "f" 'consult-fd
    "g" 'consult-ripgrep
    "o" 'ff-find-other-file
    "e" 'consult-flymake
    "." (lambda ()
          (interactive)
          (call-process-shell-command "open .")))

  :preface
  (defun +consult-recent-file ()
    (require 'consult)
    "Find recent file using `completing-read'."
    (interactive)
    (let ((default-directory (project-root-current)))
      (find-file
       (consult--read
        (or
         (mapcar
          #'consult--fast-abbreviate-file-name
          (mapcar
           (lambda (file)
             (let ((root (expand-file-name (project-root-current)))
                   (file (expand-file-name file)))
               (if (string-prefix-p root file)
                   (file-relative-name file root)
                 file)))
           (cl-remove-if
            (lambda (x) (string-prefix-p "/var/tmp/tmp" x))
            (remove
             (consult--fast-abbreviate-file-name (or (buffer-file-name) ""))
             (bound-and-true-p recentf-list)))))
         (user-error "No recent files, `recentf-mode' is %s"
                     (if recentf-mode
                         "enabled"
                       "disabled")))
        :prompt "Find recent file: "
        :sort nil
        :require-match t
        :category 'file
        :state (consult--file-preview)
        :history 'file-name-history))))

  :config
  (consult-customize
   +consult-recent-file
   consult-find
   consult-ripgrep
   consult-buffer
   :preview-key nil))

(use-builtin recentf
  :custom
  (recentf-max-saved-items 100000)
  (recentf-arrange-rules nil)
  (recentf-keep '(file-remote-p file-readable-p))
  :hook (buffer-list-update . recentf-track-opened-file)
  :config
  (recentf-mode t))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
  :init
  (setq
   completion-styles '(orderless basic)
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :demand t
  :elpaca
  (corfu :files (:defaults "extensions/*"))
  :after evil
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-scroll-margin 2)
  (corfu-count 7)
  (corfu-auto-prefix 2)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  (completion-ignore-case t)
  (corfu-bar-width 0)
  (corfu-left-margin-width 0)
  (corfu-right-margin-width 0)
  (corfu-auto-delay 0.1)

  :bind (:map corfu-map ("TAB" . nil) ([tab] . nil))

  :general
  (imap
    :keymaps 'override
    "C-n"    'completion-at-point)
  (imap
    :keymaps 'corfu-map
    "TAB" nil
    [tab] nil)

  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)

  (defun corfu--unread-this-command-keys ()
    (when (> (length (this-command-keys)) 0)
      (setq unread-command-events
            (nconc
             (listify-key-sequence (this-command-keys))
             unread-command-events))
      (clear-this-command-keys t)))

  (cl-defmethod corfu--insert :around
    (status)
    (if (or (eq this-command 'corfu-insert-exact) (not (eq status 'exact)))
        (cl-call-next-method)
      (corfu--unread-this-command-keys)
      (setq this-command 'corfu-insert-exact)))

  (defun corfu-insert-exact ()
    "Insert current candidate with the `exact' status.
Quit if no candidate is selected."
    (interactive)
    (if (>= corfu--index 0)
        (corfu--insert 'exact)
      (corfu-quit)))

  (mapc
   #'evil-declare-ignore-repeat
   '(corfu-next corfu-previous corfu-first corfu-last))

  (mapc
   #'evil-declare-change-repeat
   '(corfu-insert corfu-insert-exact corfu-complete)))

(use-package corfu-terminal
  :hook (corfu-mode . corfu-terminal-mode)
  :unless (display-graphic-p))

(use-package cape
  :demand t
  :after corfu
  :custom
  (cape-dabbrev-check-other-buffers nil)
  (cape-file-directory-must-exist nil)
  :general
  (imap "C-f" 'cape-file)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)

  (setq-default thing-at-point-file-name-chars "-@~/[:alnum:]_.$#%,:")

  (let ((dabbrev (cape-capf-case-fold #'cape-dabbrev)))
    (defun my/eglot-capf ()
      (setq-local completion-at-point-functions
                  (list
                   (cape-capf-super
                    (cape-capf-properties #'eglot-completion-at-point :exclusive 'no)
                    dabbrev)
                   #'cape-file)))
    (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

    (defun add-cape-capf ()
      (add-to-list 'completion-at-point-functions #'cape-file)
      (add-to-list 'completion-at-point-functions (cape-capf-case-fold #'cape-dabbrev)))

    (add-hook 'sh-mode-hook 'add-cape-capf)

    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions dabbrev)))

(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-C-u-scroll t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-cross-lines t)

  :general
  (nmap
    "<escape>" 'evil-ex-nohighlight)

  (nmap
    :keymaps 'evil-ex-search-keymap
    "<escape>" 'abort-recursive-edit)

  :config
  (evil-mode)

  ;; (add-hook 'evil-insert-state-entry-hook
  ;;           (lambda () (unless (eq major-mode 'vterm-mode)
  ;;                        (send-string-to-terminal "\033[5 q"))))
  ;; (add-hook 'evil-insert-state-exit-hook
  ;;           (lambda () (send-string-to-terminal "\033[2 q")))

  (evil-define-operator evil-fill (beg end)
    "Fill text."
    :move-point nil
    :type line
    (save-excursion
      (ignore-errors
        (fill-region beg end)
        (evil-indent beg end))))

  (advice-add 'evil-ex :around #'execute-at-project-root)
  (evil-set-undo-system 'undo-fu)
  (evil-ex-define-cmd "ц" 'evil-write)
  (evil-ex-define-cmd "й" 'evil-quit)

  (general-unbind 'evil-motion-state-map "TAB")
  (general-unbind 'pdf-view-mode-map "SPC")

  (evil-define-motion
    evil-next-line (count)
    :type exclusive
    (let ((line-move-visual (not count)))
      (evil-line-move (or count 1))))

  (evil-define-motion
    evil-previous-line (count)
    :type exclusive
    (let ((line-move-visual (not count)))
      (evil-line-move (- (or count 1)))))

  (evil-select-search-module 'evil-search-module 'evil-search))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-vterm-move-cursor-back t)
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-goggles
  :after evil
  :custom
  (evil-goggles-duration 0.25)
  (evil-goggles-enable-paste nil)
  (evil-goggles-enable-change nil)
  (evil-goggles-enable-delete nil)
  :config (evil-goggles-mode))

(use-package evil-commentary
  :after evil
  :init
  (evil-commentary-mode))

(use-package git-modes
  :defer t)

(use-package apheleia
  ;; :hook ((prog-mode text-mode) . apheleia-mode)
  :general
  (nvmap
    :prefix "SPC c"
    "f" 'apheleia-format-buffer)
  :config
  (rassq-delete-all 'cmake-format apheleia-mode-alist)

  (add-to-list! 'apheleia-formatters
                '(sqlfluff . ("sqlfluff" "format" "--dialect" "postgres" "-"))
                '(taplo . ("taplo" "fmt"))
                '(csharpier . ("dotnet" "csharpier"))
                '(prettier . ("prettier" "--stdin-filepath" filepath))
                '(phpcs . ("my-phpcs" "fix" "-n" "-q" filepath))
                '(taxi-black . ("taxi-black" "--quiet" "--force" filepath "-"))
                '(taxi-clang-format . ("taxi-clang-format" "--quiet" "--force" filepath "-")))

  (add-hook 'apheleia-formatter-exited-hook (cl-function
                                             (lambda (&key formatter error log)
                                               (interactive)
                                               (revert-buffer nil t))))

  (add-to-list! 'apheleia-mode-alist
                '(sql-mode . sqlfluff)
                '(python-ts-mode . taxi-black)
                '(c++-mode . taxi-clang-format)
                '(conf-toml-mode . taplo)
                '(csharp-mode . csharpier)
                '(markdown-mode . prettier)))

(use-package jinx
  :custom
  (jinx-languages "ru_RU en_US")
  (jinx-camel-modes '(prog-mode org-mode))
  :hook (text-mode . jinx-mode)
  :general
  (nvmap
    "z=" 'jinx-correct
    "]s" 'jinx-next
    "[s" 'jinx-previous))

(use-package undo-fu
  :demand t
  :after evil
  :custom
  (undo-limit 67108864) ; 64mb.
  (undo-strong-limit 100663296) ; 96mb.
  (undo-outer-limit 1006632960) ; 960mb.
  (undo-fu-allow-undo-in-region t))

(use-package undo-fu-session
  :demand t
  :after evil
  :custom
  (undo-fu-session-incompatible-files
   '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config (undo-fu-session-global-mode))

(use-package vterm
  :after evil
  :custom
  (vterm-tramp-shells '(("ssh" "/bin/zsh")))
  (vterm-copy-mode-remove-fake-newlines t)
  :general
  (nmap
    :prefix "SPC t"
    :keymaps 'override
    "" '(nil :wk "toggle")
    "t" 'toggle-vterm
    "c" 'toggle-vterm-cd
    "T" 'toggle-vterm-here)
  (nmap
    :keymaps 'vterm-mode-map
    "q" 'delete-window
    "C-p" "M-p"
    "C-n" "M-n"
    "M-:" 'eval-expression)

  :preface
  (defun toggle-vterm (&optional args)
    (require 'vterm)
    (interactive "p")
    (let* ((default-directory (project-root-current))
           (vterm-buffer-name (concat vterm-buffer-name (my-tab-name-current))))
      (if (equal major-mode 'vterm-mode)
          (let (display-buffer-alist)
            (split-window-right)
            (other-window 1)
            (vterm args))
        (vterm args))))

  (defun toggle-vterm-cd (&optional args)
    (interactive "p")
    (let ((directory (project-root-current)))
      (toggle-vterm args)
      (vterm-send "C-u")
      (vterm-send-string (concat "cd " directory))
      (vterm-send-return)
      (vterm-clear)))

  (defun toggle-vterm-here (&optional args)
    (interactive "p")
    (let (display-buffer-alist)
      (toggle-vterm args)))

  :config
  (setq vterm-timer-delay 0.01)

  (add-hook 'vterm-mode-hook (lambda () (setq-local evil-insert-state-cursor '(box))))

  (add-to-list
   'display-buffer-alist
   '((lambda (buffer-or-name _)
       (let ((buffer (get-buffer buffer-or-name)))
         (with-current-buffer buffer
           (or (equal major-mode 'vterm-mode)
               (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
     (display-buffer-reuse-window display-buffer-in-direction)
     (direction . bottom)
     (reusable-frames . visible)
     (window-height . 0.4))))

(use-package editorconfig
  :demand t
  :config (editorconfig-mode 1))

(use-package reverse-im
  :demand t
  :custom (reverse-im-input-methods '("russian-computer"))
  :config (reverse-im-mode t))

(use-package saveplace
  :elpaca nil
  :init (save-place-mode))

(use-package ls-lisp
  :elpaca nil
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil))

(use-package dired
  :elpaca nil
  :after ls-lisp
  :commands dired
  :custom
  (dired-listing-switches "-lAXGh --group-directories-first")
  (dired-auto-revert-buffer t)
  (auto-revert-verbose nil)
  (dired-kill-when-opening-new-dired-buffer t)
  :preface
  (defun +dired-open-here ()
    (interactive)
    (dired (project-root-current)))
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . dired-hide-details-mode)
  :general
  (nmap
    :keymaps 'override
    :prefix "SPC o"
    "d" 'dired
    "D" '+dired-open-here))


                                        ; (use-package auctex
                                        ;   :commands (LaTeX-mode latex-mode)
                                        ;   :custom
                                        ;   (LaTeX-item-indent 0)
                                        ;   (LaTeX-indent-level 2)
                                        ;   (tex-fontify-script nil)
                                        ;   (TeX-close-quote ">>")
                                        ;   (TeX-open-quote "<<")
                                        ;   (TeX-engine 'luatex)
                                        ;   (font-latex-fontify-script nil)
                                        ;   :custom-face
                                        ;   (font-latex-warning-face ((t :inherit 'bold)))
                                        ;   (font-latex-math-face ((t :inherit 'bold)))
                                        ;   (font-latex-string-face ((t :inherit 'font-lock-string-face)))
                                        ;   (font-latex-verbatim-face ((t :inherit 'bold)))
                                        ;   (font-latex-bold-face ((t :inherit 'bold)))
                                        ;   (font-latex-italic-face ((t :inherit 'italic)))
                                        ;   (font-latex-sectioning-0-face ((t :inherit 'bold)))
                                        ;   (font-latex-sectioning-1-face ((t :inherit 'bold)))
                                        ;   (font-latex-sectioning-2-face ((t :inherit 'bold)))
                                        ;   (font-latex-sectioning-3-face ((t :inherit 'bold)))
                                        ;   (font-latex-sectioning-4-face ((t :inherit 'bold)))
                                        ;   (font-latex-sectioning-5-face ((t :inherit 'bold)))
                                        ;   :hook (LaTeX-mode . auto-fill-mode)
                                        ;   :config
                                        ;   (add-to-list 'LaTeX-indent-environment-list '("align*")))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (face-remap-add-relative 'font-lock-type-face '(:inherit default))))

(use-package cmake-mode
  :commands cmake-mode)

(add-to-list! 'auto-mode-alist
              '("\\.latexmkrc\\'" . perl-mode)
              '("\\.h\\'" . c++-ts-mode)
              '("\\.sqlfluff\\'" . conf-mode)
              '("\\.clang-format\\'" . yaml-mode)
              '("\\.tsx\\'" . tsx-ts-mode)
              '("\\.puml\\'" . plantuml-mode)
              '("skhdrc\\'" . conf-mode))

(setq-default css-indent-offset 2)

(use-package markdown-mode
  :requires edit-indirect
  :commands markdown-mode
  :hook (markdown-mode . auto-fill-mode)
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-code-lang-modes
   '(("ocaml" . tuareg-mode)
     ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode)
     ("asymptote" . asy-mode)
     ("dot" . fundamental-mode)
     ("sqlite" . sql-mode)
     ("calc" . fundamental-mode)
     ("C" . c-ts-mode)
     ("cpp" . c++-ts-mode)
     ("C++" . c++-ts-mode)
     ("html" . mhtml-mode)
     ;; ("python" . python-ts-mode)
     ("screen" . shell-script-mode)
     ("shell" . sh-mode)
     ("bash" . sh-mode)))
  :general
  (general-define-key
   :keymaps 'markdown-mode-map
   "C-c C-c" 'markdown-toggle-gfm-checkbox)
  :config
  (defun markdown-fontify-tables (last)
    ;; (when (re-search-forward "|" last t)
    ;;   (when (markdown-table-at-point-p)
    ;;     (font-lock-append-text-property
    ;;      (line-beginning-position) (min (1+ (line-end-position)) (point-max))
    ;;      'face 'markdown-table-face))
    ;;   (forward-line 1)
    ;;   t)
    )

  (setq markdown-regex-gfm-checkbox " \\(\\[[xX-]\\]\\) "))

(use-package sql-indent
  :custom (sqlind-basic-offset 4))

(use-package ialign
  :commands ialign
  :custom (ialign-initial-repeat t)
  :general
  (nvmap
    "ga" 'ialign))

(use-builtin flymake
  :hook ((sql-mode) . flymake-mode)
  :commands flymake-mode
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-start-on-flymake-mode t)
  (flymake-no-changes-timeout 1.0)
  (flymake-start-on-save-buffer t)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-wrap-around nil)
  :general
  (nvmap
    :prefix "g"
    "n" 'flymake-goto-next-error
    "p" 'flymake-goto-prev-error)
  :config)

(use-package flymake-sqlfluff
  :elpaca (:host github :repo "danilshvalov/flymake-sqlfluff")
  :hook (sql-mode . flymake-sqlfluff-load))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package dash-modeline
  :elpaca nil
  :custom (dash-modeline-position 'dash-modeline-footer)
  :hook
  ((text-mode prog-mode) . dash-modeline-prog-mode)
  (vterm-mode . dash-modeline-term-mode)
  (messages-buffer-mode . dash-modeline-message-mode)
  (org-agenda-mode . dash-modeline-agenda-mode)
  :config
  (dash-modeline-prog-mode t)

  (with-current-buffer "*Messages*"
    (dash-modeline-message-mode)))

(use-builtin compile
  :custom
  (compilation-scroll-output t)
  (compilation-read-command nil)
  :general
  (nvmap
    :prefix "SPC c"
    "c" 'compile)
  :config
  (defadvice compile (after jump-back activate)
    (other-window 1))

  (add-hook
   'LaTeX-mode-hook
   (lambda () (setq-local compile-command "latexmk"))))

(use-builtin help
  :general
  (nvmap "SPC h" `(,(general-simulate-key "C-h") :wk "+help")))

(use-package markdown-preview-mode
  :commands markdown-preview-mode
  :config
  (setq markdown-preview-stylesheets
        (list
         "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
         "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css"
         "
  <style>
   .markdown-body {
     box-sizing: border-box;
     min-width: 200px;
     max-width: 980px;
     margin: 0 auto;
     padding: 45px;
   }

   @media (max-width: 767px) {
     .markdown-body {
       padding: 15px;
     }
   }
  </style>
"))
  (setq markdown-preview-javascript
        (list
         "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
         "
  <script>
   $(document).on('mdContentChange', function() {
     $('pre code').each(function(i, block) {
       hljs.highlightBlock(block);
     });
   });
  </script>
")))

(use-package treesit-auto
  :demand t
  :config
  (setq my-sql-tsauto-config
        (make-treesit-auto-recipe
         :lang 'sql
         :ts-mode 'sql-ts-mode
         :remap 'sql-mode
         :url "https://github.com/DerekStride/tree-sitter-sql"
         :revision "gh-pages"
         :source-dir "src"))

  (add-to-list 'treesit-auto-recipe-list my-sql-tsauto-config)

  (global-treesit-auto-mode))

(use-package embark
  :commands embark-act
  :general
  (:states '(normal visual insert)
           :keymaps 'override
           "C-e" 'embark-act)
  (general-define-key
   :keymaps 'embark-file-map
   "C-v" 'split-window-vertically)
  (imap
    :keymaps 'vertico-map
    "C-v" "C-e C-v"
    "C-q" "C-e S")
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (setq embark-indicators
        '(embark--vertico-indicator
          embark-minimal-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package image-mode
  :elpaca nil
  :commands image-mode
  :custom (image-use-external-converter t))

(when (display-graphic-p)
  (set-fringe-style -1))

(use-package pbcopy
  :when (equal system-type 'darwin)
  :init (turn-on-pbcopy))

(setq cc-search-directories '("."
                              "../include" "../include/*" "../../include/*"
                              "../../../include/*" "../../include/*/*"
                              "../../../include/*/*/*" "../src" "../src/*"
                              "../../src/*" "../../../src/*"
                              "../../src/*/*" "../../../src/*/*/*"
                              "/usr/include" "/usr/local/include/*"))

(with-eval-after-load 'highlight-doxygen
  (defun highlight-doxygen-anchored-keywords-template ()
    "List of font-lock keywords that will be converted to anchored submatches.

The MATCHER will be wrapped in a call to
`highlight-doxygen-forward-search' and pre and post match forms
will be added.

Note that these rules can't contain anchored rules themselves."
    (let ((title-rules '()))
      (dolist (pair highlight-doxygen-title-commands-alist)
        (let ((commands (car pair))
              (face     (cdr pair)))
          (push `(,(concat "[\\@]\\_<"
                           (regexp-opt commands)
                           "\\s-+"
                           "\\(.*\\)")
                  (1 (quote ,face) prepend))
                title-rules)))
      (dolist (pair highlight-doxygen-name-title-commands-alist)
        (let ((commands (car pair))
              (face     (cdr pair)))
          (push `(,(concat "[\\@]\\_<"
                           (regexp-opt commands)
                           "\\s-+"
                           "\\_<\\(\\sw+\\)"
                           "\\(\\s-+"
                           "\\(.*\\)\\)?")
                  (1 'highlight-doxygen-label prepend)
                  (2 (quote ,face) prepend t))
                title-rules)))
      (append
       `(
         ;; --------------------
         ;; Highlight every line in the Doxygen block.
         ;;
         ;; Unlike plain comment highlighting, make the highlighting
         ;; follow the indentation of the Doxygen comment.
         (highlight-doxygen-match-comment-line
          (0 'highlight-doxygen-comment prepend))
         ;; --------------------
         ;; Explicit code blocks
         (highlight-doxygen-find-and-highlight-keywords-code-block)
         ;; --------------------
         ;; Implicit (indented) code blocks
         (highlight-doxygen-find-and-highlight-markdown-code-block)
         ;; --------------------
         ;; Doxygen command.
         (,(concat "[\\@]"
                   "\\_<\\([a-z]+\\)\\_>")
          (1 'highlight-doxygen-command prepend))

         ;; ----------------------------------------
         ;; Inline constructs.

         ;; --------------------
         ;; Type name

         (highlight-doxygen-match-camel-case
          (1 font-lock-type-face prepend))

         ;; --------------------
         ;; Qualified class name

         ("\\_<\\(\\sw+\\)\\(::\\|#\\)"
          (1 font-lock-type-face prepend))

         ;; --------------------
         ;; Function name
         ("\\_<\\(\\(\\sw\\)+\\)()"
          (1 font-lock-function-name-face prepend))

         ;; --------------------
         ;; Links (URI:s). See RFC 3986, chapter 3.

         ("\\_<\\([a-zA-Z][-a-zA-Z0-9+.]*://[^ \t\n]*\\)"
          (1 'highlight-doxygen-link prepend)))
       title-rules
       `(
         ;; ------------------------------
         ;; Various command signatures.
         ;;

         ;; --------------------
         ;; bold
         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-bold-commands)
                   "\\s-+"
                   "\\_<\\(\\sw+\\)")
          (1 'highlight-doxygen-bold prepend))

         ;; --------------------
         ;; code
         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-code-commands)
                   "\\s-+"
                   "\\_<\\(\\sw+\\)")
          (1 'highlight-doxygen-code prepend))

         ;; --------------------
         ;; emphasize
         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-emphasize-commands)
                   "\\s-+"
                   "\\_<\\(\\sw+\\)")
          (1 'highlight-doxygen-emphasize prepend))

         ;; --------------------
         ;; Type name

         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-qualified-type-commands)
                   "\\s-+"
                   ;; Skip qualifiers.
                   "\\_<\\(?:\\sw+\\(?:::\\|#\\)\\)*"
                   "\\_<\\(\\sw+\\)")
          (1 'highlight-doxygen-type prepend))

         ;; --------------------
         ;; exception

         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-exception-commands)
                   "\\s-+"
                   ;; Skip qualifiers.
                   "\\_<\\(?:\\sw+\\(?:::\\|#\\)\\)*"
                   "\\(\\sw+\\)")
          (1 'highlight-doxygen-exception prepend))

         ;; --------------------
         ;; namespace

         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-namespace-commands)
                   "\\s-+"
                   ;; Skip qualifiers.
                   "\\_<\\(?:\\sw+\\(?:::\\|#\\)\\)*"
                   "\\_<\\(\\sw+\\)")
          (1 'highlight-doxygen-namespace prepend))

         ;; --------------------
         ;; Group name
         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-group-commands)
                   "\\s-+"
                   "\\_<\\(\\sw+\\)")
          (1 'highlight-doxygen-group prepend))

         ;; --------------------
         ;; File name
         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-filename-commands)
                   "\\s-+"
                   "\\_<\\([a-zA-Z0-9_:/\\.]+\\)")
          (1 'highlight-doxygen-filename prepend))

         ;; --------------------
         ;; Reference

         ;; Note: The Doxygen documentation doesn't specify the format
         ;; of a reference, this code use a combination of word
         ;; characters, symbol characters, and punctuation
         ;; characters. Another approach would be to match every
         ;; character except whitespace.  Unfortunately, "\\S-" might
         ;; match newlines, so the search must be restricted to the end
         ;; of the line that contains the Doxygen command.
         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-reference-commands)
                   "\\s-+"
                   "\\(\\(\\sw\\|\\s_\\|\\s.\\)+\\)")
          (1 'highlight-doxygen-link prepend))

         ;; --------------------
         ;; section-label (`if' and `elseif' etc.)

         ;; TODO: The section label can be a complex expression like
         ;; "(TEST1 && !TEST2). Since this is rule itself is included in a
         ;; anchored match, it's not possible to handle this using anchored
         ;; matches, so it must be done in elisp.
         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-section-label-commands)
                   "\\s-+"
                   "\\_<\\(\\sw+\\)")
          (1 'highlight-doxygen-section-label prepend))

         ;; --------------------
         ;; Variable

         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-variable-commands)
                   "\\s-+"
                   "\\_<\\(\\(\\sw\\|_\\)+\\)")
          (1 'highlight-doxygen-variable prepend))

         ;; --------------------
         ;; Variable with direction

         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-variable-with-dir-commands)
                   "\\_>"
                   "\\s-*"
                   "\\(?:\\["
                   "\\(?:\\(in\\)\\|\\(out\\)\\|\\(in\\),\\(out\\)\\)"
                   "\\]\\)?"
                   "\\s-*"
                   "\\(\\_<\\(\\sw\\|_\\)+\\)?")
          (1 'highlight-doxygen-direction prepend t) ; in
          (2 'highlight-doxygen-direction prepend t) ; out
          (3 'highlight-doxygen-direction prepend t) ; in  (part of in,out)
          (4 'highlight-doxygen-direction prepend t) ; out (part of in,out)
          (5 'highlight-doxygen-variable prepend t))

         ;; --------------------
         ;; Line of code

         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-code-line-commands)
                   "\\s-+\\(.*\\)$")
          (0 (progn
               (highlight-doxygen-code-block
                (match-beginning 1)
                (match-end 1)
                major-mode)
               nil)))

         ;; --------------------
         ;; Reference or file name

         (,(concat "[\\@]\\_<"
                   (regexp-opt highlight-doxygen-link-object-commands)
                   "\\_>")
          (0 (progn
               ;; This will apply suitable highlighting to whatever is
               ;; after the command.
               (highlight-doxygen-highlight-link-object)
               nil)))

         ;; --------------------
         ;; Highlight "`foo`". Note that in Doxygen a quote cancels a
         ;; backquote.
         ;;
         ;; TODO: Multi-line support.
         ("`\\([^\n`']+\\)`"
          (1 (progn
               (goto-char (match-end 0))
               font-lock-constant-face)
             prepend)))))))

(use-package lua-mode
  :mode "\\.lua\\'")

;; (use-builtin tramp
;;   :config
;;   (setopt
;;    explicit-shell-file-name "/bin/zsh"
;;    tramp-default-remote-shell "/bin/zsh"
;;    tramp-encoding-shell "/bin/zsh"
;;    tramp-verbose 0))

(use-builtin calendar
  :defer t
  :custom
  (calendar-minimum-window-height 10))

;; (use-package smartparens
;;   :demand t
;;   :general
;;   (imap "C-M-i" 'sp-up-sexp)
;;   :config
;;   (smartparens-global-mode t)

;;   (dolist (prefix '("\\Big" "\\Bigg" "\\big" "\\bigg"))
;;     (dolist (suffixes '(("(" . ")") ("{" . "}") ("[" . "]") ("|" . "|")))
;;       (sp-local-pair 'latex-mode
;;                      (concat prefix (car suffixes))
;;                      (concat prefix (cdr suffixes)))))

;;   (sp-with-modes '(tex-mode
;;                    plain-tex-mode
;;                    latex-mode
;;                    LaTeX-mode)
;;     (sp-local-pair "\\{" "\\}")
;;     (sp-local-pair "\\[" "\\]"))

;;   (sp-with-modes 'org-mode
;;     (sp-local-pair "~" "~")))

(use-builtin treesit
  :custom
  (treesit-font-lock-level 4)
  (major-mode-remap-alist
   '((c++-mode . c++-ts-mode)
     (c-mode . c-ts-mode))))

(use-package rainbow-mode
  :general
  (nvmap
    :prefix "SPC t"
    "r" 'rainbow-mode))

(use-builtin eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-builtin vc
  :defer t
  :custom
  (vc-handled-backends nil))

(add-hook
 'yaml-ts-mode-hook
 (lambda ()
   (setq-local tab-width 2)))

(use-package powershell
  :custom
  (powershell-indent 2))

(use-builtin mu4e
  :commands (mu4e mu4e-search)
  :custom
  (mu4e-completing-read-function 'completing-read)
  (shr-use-colors nil)
  :general
  (nmap
    :prefix "SPC m"
    "m" 'mu4e)
  :config
  (mu4e--init-handlers))

(defun my-minibuffer-save-history ()
  (interactive)
  (let ((content (minibuffer-contents))
        (prompt (minibuffer-prompt)))
    (cond
     ((string-prefix-p "#" content)
      (add-to-history 'consult--find-history content)
      (add-to-history 'consult--grep-history content))
     (read-extended-command-mode (add-to-history 'extended-command-history content))
     ((member prompt '(":")) (add-to-history 'evil-ex-history content))
     ((member prompt '("/" "?")) (add-to-history 'evil-ex-search-history content))
     ((equal prompt "Eval: ") (add-to-history 'read-expression-history content)))))

(add-hook 'minibuffer-exit-hook 'my-minibuffer-save-history)

(use-package plantuml-mode
  :custom
  (plantuml-indent-level 2))

(use-package php-mode)

(use-builtin tab-bar
  :custom
  (tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
  (tab-bar-tab-hints t)
  (tab-bar-close-button-show nil)
  (tab-bar-auto-width nil)
  (tab-bar-show 1)
  (tab-bar-new-tab-to 'rightmost)
  :preface
  (defun my-tab-name-current ()
    "1"
    ;; (cdr (assoc 'name (cdr (tab-bar--current-tab-find nil nil))))
    )

  :config
  (tab-bar-rename-tab (number-to-string (random)))
  (advice-add
   'tab-bar-new-tab
   :around
   (lambda (fun &rest args)
     (apply fun args)
     (tab-bar-rename-tab (number-to-string (random)))
     (dired (project-root-current))))

  (defun tab-bar-tab-name-format-default (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (if tab-bar-tab-hints (format " %d " i) "")
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   ""))
       'face (funcall tab-bar-tab-face-function tab)))))

(use-package dockerfile-mode)

(use-package nginx-mode)

(defun markdown-ts-query-blocks ()
  (let* ((parser (treesit-parser-create 'markdown))
         (root (treesit-parser-root-node parser))
         (query '((fenced_code_block
                   (info_string) @lang
                   (code_fence_content) @content)))
         (captures (treesit-query-capture root query))
         (i 0))
    (while (< i (length captures))
      (let* ((lang-node (cdr (nth i captures)))
             (lang (treesit-node-text lang-node))
             (lang-mode (if lang (markdown-get-lang-mode lang)
                          markdown-fontify-code-block-default-mode))
             (content-node (cdr (nth (1+ i) captures)))
             (content (treesit-node-text content-node))
             (start (treesit-node-start content-node))
             (end (treesit-node-end content-node)))

        (let ((string content)
              (modified (buffer-modified-p))
              (markdown-buffer (current-buffer))
              pos next)
          (remove-text-properties start end '(face nil))
          (with-current-buffer
              (get-buffer-create
               (concat " markdown-code-fontification:" (symbol-name lang-mode)))
            ;; Make sure that modification hooks are not inhibited in
            ;; the org-src-fontification buffer in case we're called
            ;; from `jit-lock-function' (Bug#25132).
            (let ((inhibit-modification-hooks nil))
              (delete-region (point-min) (point-max))
              (insert string " ")) ;; so there's a final property change
            (unless (eq major-mode lang-mode) (funcall lang-mode))
            (font-lock-ensure)
            (setq pos (point-min))
            (while (setq next (next-single-property-change pos 'face))
              (let ((val (get-text-property pos 'face)))
                (when val
                  (message "%s" `(put-text-property
                                  ,(+ start (1- pos)) ,(1- (+ start next)) 'face
                                  ,val ,markdown-buffer))
                  (put-text-property
                   (+ start (1- pos)) (1- (+ start next)) 'font-lock-face
                   val markdown-buffer)))
              (setq pos next)))
          (add-text-properties
           start end
           '(font-lock-fontified t fontified t font-lock-multiline t))
          (set-buffer-modified-p modified))
        ;; (message "%s" lang-mode)
        ;; (message "%s" content)
        ;; (message "%s" "------------")
        )
      (setq i (+ i 2)))

    )
  ;; (treesit-update-ranges)
  )


(defmacro markdown-ts-capture (parser-language parser-mode)
  `(lambda (beg end)
     (let* ((parser (treesit-parser-create 'markdown))
            (root (treesit-parser-root-node parser))
            (query '((fenced_code_block
                      (info_string) @lang
                      (code_fence_content) @content)))
            (captures (treesit-query-capture root query beg end))
            (set-ranges)
            (i 0))
       (while (< i (length captures))
         (let* ((lang-node (cdr (nth i captures)))
                (lang (treesit-node-text lang-node))
                (lang-mode (if lang (markdown-get-lang-mode lang)
                             markdown-fontify-code-block-default-mode))
                (content-node (cdr (nth (1+ i) captures))))
           (if (eq lang-mode ,parser-mode)
               (setq set-ranges (push (cons (treesit-node-start content-node)
                                            (treesit-node-end content-node))
                                      set-ranges))))
         (setq i (+ i 2)))
       (when set-ranges
         (treesit-parser-set-included-ranges
          (treesit-parser-create ,parser-language) (nreverse set-ranges))))))

(use-builtin files
  :general
  (nmap
    :keymaps 'override
    :prefix "SPC c"
    "d" (lambda ()
          (interactive)
          (cd default-directory))
    "D" 'cd)
  :config
  ;; (advice-add
  ;;  'cd
  ;;  :around
  ;;  (lambda (fun &rest args)
  ;;    (apply fun args)
  ;;    (my-set-current-directory (car args))
  ;;    (message "Directory: %s" default-directory)))
  )

(use-package web-mode
  :custom
  (web-mode-enable-auto-expanding t)
  (web-mode-enable-auto-closing t)
  :config
  (add-hook 'mhtml-mode-hook 'web-mode))

(use-package git-commit)

(imap "<backtab>" "C-d")
(advice-add 'indent-for-tab-command
            :around
            (lambda (fun &rest args)
              (if (eq evil-state 'insert)
                  (tab-to-tab-stop)
                (apply fun args))))

(use-package clipetty
  :demand t
  :config
  (global-clipetty-mode))

(use-package with-editor
  :demand t
  :config
  (add-hook 'vterm-mode-hook 'with-editor-export-editor))

(define-advice server-eval-and-print (:filter-args (args) no-print)
  (list (car args) nil))

(defun obsidian-daily-note (&optional day-offset)
  (interactive)
  (let* ((day-offset (or day-offset (read-number "Enter day offset: " 0)))
         (obsidian-root "~/obsidian")
         (daily-note-directory "ежедневник")
         (daily-note-root (file-name-concat obsidian-root daily-note-directory))
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
         (filename (read-string "Enter filename: "))
         (filename (downcase filename))
         (filename (replace-regexp-in-string "[[:space:]]+" "-" filename))
         (filename (replace-regexp-in-string "[^[:alnum:]-/]" "" filename))
         (filename (file-name-with-extension filename "md"))
         (note-path (file-name-concat obsidian-root filename)))
    (find-file note-path)
    (make-directory (file-name-directory note-path) t)
    (write-file note-path)))
