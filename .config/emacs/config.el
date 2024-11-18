;; -*-lexical-binding: t -*-

(require 'my-utils)

(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-auto-unbind-keys t)
  (general-evil-setup t))

(use-builtin emacs
  :custom
  (confirm-kill-processes nil)
  (scroll-margin 10)
  (hscroll-margin 20)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (split-width-threshold nil)
  (ring-bell-function 'ignore)
  (fill-column 80)
  (use-short-answers t)
  (ns-right-option-modifier nil)
  (make-backup-files nil)
  (auto-save-default nil)
  (whitespace-style '(face tabs))
  (window-combination-resize t)
  (help-window-select t)
  (bidi-display-reordering nil)
  (sentence-end-double-space nil)
  (history-length t)
  (visible-cursor nil)
  (warning-minimum-level :emergency)
  (initial-major-mode 'text-mode)
  (initial-scratch-message "")
  (enable-recursive-minibuffers t)
  (read-process-output-max (* 1024 1024))
  :hook
  ((prog-mode text-mode) . display-fill-column-indicator-mode)
  :preface
  ;; remove image resize delay
  (advice-add 'image--delayed-change-size :override 'image--change-size)

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
  :config
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq buffer-file-coding-system 'utf-8-unix)

  (when (display-graphic-p)
    (setopt confirm-kill-emacs #'y-or-n-p))

  (add-hook 'window-configuration-change-hook
            (lambda ()
              (unless (display-graphic-p)
                (require 'disp-table)
                (let ((display-table (or buffer-display-table
                                         standard-display-table)))
                  (set-display-table-slot display-table 'vertical-border ?│)))))

  ;; enable mouse in terminal
  (xterm-mouse-mode)

  (menu-bar-mode -1)

  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (global-whitespace-mode +1)
  (savehist-mode)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(use-package doom-themes
  :config
  (load-theme 'doom-tokyonight-storm t)

  (defface obsidian-tag
    `((t (:foreground ,(doom-color 'cyan))))
    "Any documentation."
    :group nil)

  (doom-themes-set-faces 'user
    `(highlight
      :foreground 'unspecified
      :background "#3C435E"
      :inherit 'region)
    `(font-lock-comment-face :foreground ,(doom-lighten 'comments 0.2))
    `(corfu-current :background ,(doom-color 'region))
    `(evil-snipe-matches-face
      :foreground 'unspecified
      :background ,(doom-color 'region)
      :underline nil)
    `(highlight-doxygen-comment :background 'unspecified)
    `(hl-line :background ,(doom-lighten 'bg 0.05))
    `(highlight-doxygen-code-block :background 'unspecified)
    `(markdown-ts-code-face :background 'unspecified)
    `(markdown-ts-list-face :foreground ,(doom-color 'yellow))
    `(font-lock-operator-face :foreground ,(doom-color 'operators))
    `(font-lock-punctuation-face :foreground ,(doom-color 'punctuations))
    `(tab-bar-tab :background ,(doom-color 'region))
    `(show-paren-mismatch :foreground ,(doom-color 'red) :background 'unspecified)
    `(jinx-misspelled :underline `(:style wave :color ,(doom-color 'yellow)))
    `(markdown-ts-heading-1-face :foreground ,(doom-color 'blue))
    `(markdown-ts-heading-2-face :foreground ,(doom-color 'yellow))
    `(markdown-ts-heading-3-face :foreground ,(doom-color 'green))
    `(markdown-ts-heading-4-face :foreground ,(doom-color 'teal))
    `(markdown-ts-heading-5-face :foreground ,(doom-color 'magenta))
    `(markdown-ts-heading-6-face :foreground ,(doom-color 'violet))
    `(markdown-ts-bold-face :foreground ,(doom-color 'orange) :bold t)
    `(markdown-ts-italic-face :foreground ,(doom-color 'violet) :italic t)
    `(markdown-ts-inline-code-face :background "#414868" :foreground ,(doom-color 'blue))
    `(anzu-mode-line :foreground ,(doom-color 'fg))
    `(evil-goggles-default-face :foreground ,(doom-color 'bg) :background ,(doom-color 'yellow))
    `(flymake-end-of-line-diagnostics-face :height 'unspecified :box 'unspecified)
    `(vertical-border :foreground ,(doom-color 'yellow) :background 'unspecified)
    `(eglot-diagnostic-tag-unnecessary-face :underline t)))

(defun concat-lines (&rest args)
  (string-join args "\n"))

(defun dired-project-root ()
  (interactive)
  (let ((default-directory (project-root-current)))
    (dired default-directory)))

(cl-defmethod project-root ((project (head local)))
  "Return root directory of current PROJECT."
  (cdr project))

(defun project-find-root (dir)
  "Determine if DIR is a non-VC project.
DIR must include a .project file to be considered a project."
  (cons 'local (project-root-current)))

(defun my-project-add (&optional dir)
  (interactive)
  (let ((dir (or dir
                 (read-directory-name "Remember project at: "
                                      default-directory
                                      default-directory
                                      t))))
    (project-remember-project (cons 'local dir))))

(add-hook 'project-find-functions #'project-find-root)

(set-frame-font "JetBrains Mono 17" nil t)

(custom-set-faces
 `(default ((t (:font "JetBrains Mono 17"))))
 `(fixed-pitch ((t (:inherit (default)))))
 `(fixed-pitch-serif ((t (:inherit (default)))))
 `(variable-pitch ((t (:inherit (default)))))
 `(variable-pitch-text ((t (:height 1.0)))))

(add-hook 'c++-ts-mode-hook (lambda () (setq c-ts-mode-indent-offset 4)))

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

(use-builtin adaptive-wrap
  :hook
  ((prog-mode text-mode vterm-mode) . visual-line-mode)
  (visual-line-mode . +adaptive-wrap-prefix-mode)
  (calendar-mode . +disable-visual-line-mode)
  :init
  (defun +adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (when (fboundp 'adaptive-wrap-prefix-mode)
      (unless
          (or (equal major-mode 'org-mode) (equal major-mode 'org-journal-mode))
        (adaptive-wrap-prefix-mode
         (if visual-line-mode 1 -1)))))

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
  (electric-pair-mode t)

  (add-to-list 'electric-pair-pairs (cons ?« ?»)))

(use-package yasnippet
  :hook
  (org-mode . (lambda () (yas-activate-extra-mode 'latex-mode)))
  (evil-insert-state-entry . yas-minor-mode)
  :config
  (setq yas-verbosity 2)
  (setq yas-indent-line 'fixed)
  (setq yas-triggers-in-field t)
  (yas-reload-all)

  (defun yas--expand-or-prompt-for-template (templates &optional start end)
    "Expand one of TEMPLATES from START to END.

Prompt the user if TEMPLATES has more than one element, else
expand immediately.  Common gateway for
`yas-expand-from-trigger-key' and `yas-expand-from-keymap'."
    (let ((yas--current-template
           (cdar templates)))
      (when yas--current-template
        (yas-expand-snippet yas--current-template start end)))))

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
  (nvmap
    :keymaps 'override
    "s" #'avy-goto-char-2))

(use-builtin eglot
  :defer t
  :custom
  (eglot-sync-connect nil)
  (eglot-events-buffer-size 0)
  (eglot-send-changes-idle-time 0.1)
  :hook
  ((;;LaTeX-mode
    latex-mode
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
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  (fset #'jsonrpc--log-event #'ignore)

  (fset #'eglot--snippet-expansion-fn #'ignore)

  (add-to-list 'eglot-server-programs '(c++-ts-mode . ("clangd" "--compile-commands-dir=build_debug")))
  (add-to-list 'eglot-server-programs '(latex-mode . ("texlab")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp")))
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
  (consult-async-split-style 'semicolon)
  (consult-preview-key nil)
  (consult-async-min-input 0)
  (consult-async-refresh-delay 0.01)
  (consult-async-input-debounce 0.01)
  (consult-async-input-throttle 0.01)
  (consult-find-args "find . -not ( -path */__pycache__ -prune ) -not ( -path */.mypy_cache -prune ) -not ( -path */build_debug -prune )")
  :general
  (nvmap
    :keymaps 'override
    :prefix "SPC f"
    "" '(nil :wk "find")
    "r" #'my-consult-recent-file
    "b" #'consult-buffer
    "f" #'consult-find
    "F" #'consult-find-at
    "g" #'consult-ripgrep
    "G" #'consult-ripgrep-at
    "o" #'ff-find-other-file
    "e" #'consult-flymake
    "." (lambda ()
          (interactive)
          (call-process-shell-command "open .")))

  (nvmap
    :keymaps 'override
    :prefix "SPC b"
    "f" #'consult-bookmark
    "e" #'edit-bookmarks)

  :preface
  (defun consult-find-at ()
    (interactive)
    (let ((directory (read-directory-name "Directory: ")))
      (consult-find directory)))

  (defun consult-ripgrep-at ()
    (interactive)
    (let ((directory (read-directory-name "Directory: ")))
      (consult-ripgrep directory)))

  (defun my-consult-recent-file ()
    "Find recent file using `completing-read'."
    (interactive)
    (require 'consult)
    (let ((default-directory (project-root-current)))
      (unless (bound-and-true-p recentf-list)
        (user-error "No recent files, `recentf-mode' is %s"
                    (if recentf-mode
                        "enabled"
                      "disabled")))
      (find-file
       (consult--read
        (mapcar
         (lambda (file)
           (let ((root (consult--fast-abbreviate-file-name (project-root-current)))
                 (file (consult--fast-abbreviate-file-name file)))
             (if (string-prefix-p root file)
                 (file-relative-name file root)
               file)))
         (cl-remove-if
          (lambda (x)
            (or
             (string-prefix-p "/var/tmp/tmp" x)
             (string-prefix-p "/private/var/folders" x)
             (not (file-exists-p x))))
          (remove
           (abbreviate-file-name (or (buffer-file-name) ""))
           (mapcar
            #'consult--fast-abbreviate-file-name
            (bound-and-true-p recentf-list)))))
        :prompt "Find recent file: "
        :sort nil
        :require-match t
        :category 'file
        :state (consult--file-preview)
        :history 'file-name-history))
      )
    ;; (recentf-track-opened-file)
    )
  :config
  (defun consult--find (prompt builder initial)
    "Run find command in current directory.

The function returns the selected file.
The filename at point is added to the future history.

BUILDER is the command line builder function.
PROMPT is the prompt.
INITIAL is initial input."
    (consult--read
     (consult--async-command builder
       (consult--async-map (lambda (x) (string-remove-prefix "./" x)))
       (consult--async-highlight builder)
       :file-handler t) ;; allow tramp
     :prompt prompt
     :sort t
     :require-match t
     :initial (consult--async-split-initial initial)
     :add-history (consult--async-split-thingatpt 'filename)
     :category 'file
     :history '(:input consult--find-history))))

(use-builtin recentf
  :custom
  (recentf-max-saved-items 100000)
  (recentf-arrange-rules nil)
  (recentf-auto-cleanup 'never)
  (recentf-keep nil)
  :hook
  (window-configuration-change . recentf-track-opened-file)
  :config
  (recentf-mode t))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
  :init
  (setq
   completion-styles '(orderless basic)
   completion-category-defaults nil
   completion-category-overrides '((file (styles orderless partial-completion)))))

(use-package corfu
  :ensure (corfu :files (:defaults "extensions/*"))
  :after evil
  :custom
  (global-corfu-minibuffer nil)
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
  :hook (evil-insert-state-entry . global-corfu-mode)

  :general
  (imap
    :keymaps 'override
    "C-n"    'completion-at-point)
  (imap
    :keymaps 'corfu-map
    "TAB" nil
    [tab] nil)

  :config
  (require 'cape)

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
  :custom
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
      (add-to-list! 'completion-at-point-functions dabbrev))

    (add-hook 'sh-mode-hook 'add-cape-capf)

    (add-cape-capf)))

(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-C-u-scroll t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-fine-undo t)
  (setq evil-shift-round nil)

  :general
  (nmap
    "<escape>" 'evil-ex-nohighlight)

  (nmap
    :keymaps 'evil-ex-search-keymap
    "<escape>" 'abort-recursive-edit)

  :config
  (evil-mode)

  (evil-define-command my-evil-edit-arcadia (file)
    :repeat nil
    (interactive
     (list (read-from-minibuffer "Arcadia path: " "~/arcadia/")))
    (evil-edit file))

  (evil-ex-define-cmd "ea" 'my-evil-edit-arcadia)

  (when (display-graphic-p)
    (nvmap
      :keymaps 'override
      "M-[" #'tab-previous
      "M-]" #'tab-next
      "M-{" #'tab-move-previous
      "M-}" #'tab-move-next
      "SPC c t" #'tab-new))

  (unless (display-graphic-p)
    (add-hook 'evil-insert-state-entry-hook
              (lambda () (unless (eq major-mode 'vterm-mode)
                           (send-string-to-terminal "\033[5 q"))))
    (add-hook 'evil-insert-state-exit-hook
              (lambda () (send-string-to-terminal "\033[2 q"))))

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
  (evil-goggles-duration 0.2)
  (evil-goggles-enable-paste nil)
  (evil-goggles-enable-change nil)
  (evil-goggles-enable-delete nil)
  :config (evil-goggles-mode))

(use-package evil-commentary
  :after evil
  :init
  (evil-commentary-mode))

(use-package apheleia
  :defer t
  :general
  (nvmap
    :prefix "SPC c"
    "f" (lambda ()
          (interactive)
          (save-buffer)
          (call-interactively 'apheleia-format-buffer)))
  :config
  (rassq-delete-all 'cmake-format apheleia-mode-alist)

  (add-hook 'apheleia-formatter-exited-hook (lambda (&rest args) (revert-buffer :ignore-auto :noconfirm)))

  (add-to-list! 'apheleia-formatters
                '(sqlfluff . ("sqlfluff" "format" "--dialect" "postgres" "-"))
                '(taplo . ("taplo" "fmt"))
                '(csharpier . ("dotnet" "csharpier"))
                '(prettier . ("prettier" "--stdin-filepath" filepath))
                '(markdownlint . ("markdownlint" "--quiet" "--fix" filepath))
                '(phpcs . ("my-phpcs" "fix" "-n" "-q" filepath))
                '(yamlfmt . ("yamlfmt" filepath))
                '(taxi-format . ("taxi-format-quiet" "--quiet" filepath)))

  (add-to-list! 'apheleia-mode-alist
                '(sql-mode . sqlfluff)
                '(python-ts-mode . taxi-format)
                '(c++-mode . taxi-format)
                '(conf-toml-mode . taplo)
                '(csharp-mode . csharpier)
                '(xml-mode . xmllint)
                '(nxml-mode . xmllint)
                '(json-ts-mode . taxi-format)
                '(yaml-ts-mode . yamlfmt)
                ;; '(yaml-ts-mode . taxi-format)
                '(markdown-ts-mode . prettier)))

(use-package jinx
  :custom
  (jinx-languages "ru_RU en_US")
  (jinx-camel-modes '(prog-mode text-mode))
  :hook ((text-mode markdown-ts-mode) . jinx-mode)
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
  (undo-fu-session-make-file-name-function 'my-undo-fu-session-make-file-name)
  :config (undo-fu-session-global-mode)
  :preface
  (defun my-undo-fu-session-make-file-name (filename ext)
    "Take the path FILENAME, EXT and return a name base on this."
    (declare (important-return-value t) (side-effect-free error-free))
    (concat
     (file-name-concat undo-fu-session-directory
                       (string-replace
                        "/"
                        "-"
                        (convert-standard-filename (expand-file-name filename))))
     ext)))

(use-package vterm
  :after evil
  :custom
  (vterm-tramp-shells '(("ssh" "/bin/zsh")
                        ("sshx" "/bin/zsh")))
  (vterm-max-scrollback 100000)
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
    (interactive "p")
    (require 'vterm)
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
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

  (add-hook 'evil-insert-state-entry-hook (lambda ()
                                            (when (eq major-mode 'vterm-mode)
                                              (vterm-reset-cursor-point))))

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
     (window-height . 0.25))))

(use-builtin editorconfig
  :demand t
  :config (editorconfig-mode 1))

(use-package reverse-im
  :demand t
  :custom (reverse-im-input-methods '("russian-computer"))
  :config (reverse-im-mode t))

(use-builtin saveplace
  :init (save-place-mode))

(use-builtin ls-lisp
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil))

(use-builtin dired
  :after ls-lisp
  :commands dired
  :custom
  (dired-listing-switches "-lAXGh --group-directories-first")
  (dired-auto-revert-buffer t)
  (auto-revert-verbose nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-movement-style 'bounded)
  :preface
  (defun +dired-open-here ()
    (interactive)
    (dired (project-root-current)))
  (defun my-evil-dired-change ()
    (interactive)
    (dired-toggle-read-only)
    (call-interactively 'evil-change))
  (defun my-dired-setup ()
    (nmap dired-mode-map
      "c" 'my-evil-dired-change))
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . dired-hide-details-mode)
  (dired-mode . my-dired-setup)
  :general
  (nmap
    :keymaps 'override
    :prefix "SPC"
    "e" (lambda ()
          (interactive)
          (dired default-directory))
    "E" '+dired-open-here))

(use-package auctex
  ;; :ensure (auctex :pre-build (("./autogen.sh")
  ;;                             ("./configure"
  ;;                              "--without-texmf-dir"
  ;;                              "--with-packagelispdir=./"
  ;;                              "--with-packagedatadir=./")
  ;;                             ("make"))
  ;;                 :build (:not elpaca--compile-info) ;; Make will take care of this step
  ;;                 :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style"))
  ;; :commands (LaTeX-mode latex-mode)
  :custom
  (LaTeX-item-indent 0)
  (LaTeX-indent-level 2)
  (tex-fontify-script nil)
  (TeX-close-quote ">>")
  (TeX-open-quote "<<")
  (tex-close-quote ">>")
  (tex-open-quote "<<")
  (TeX-engine 'luatex)
  (font-latex-fontify-script nil)
  :custom-face
  (font-latex-warning-face ((t :inherit 'bold)))
  (font-latex-math-face ((t :inherit 'bold)))
  (font-latex-string-face ((t :inherit 'font-lock-string-face)))
  (font-latex-verbatim-face ((t :inherit 'bold)))
  (font-latex-bold-face ((t :inherit 'bold)))
  (font-latex-italic-face ((t :inherit 'italic)))
  (font-latex-sectioning-0-face ((t :inherit 'bold)))
  (font-latex-sectioning-1-face ((t :inherit 'bold)))
  (font-latex-sectioning-2-face ((t :inherit 'bold)))
  (font-latex-sectioning-3-face ((t :inherit 'bold)))
  (font-latex-sectioning-4-face ((t :inherit 'bold)))
  (font-latex-sectioning-5-face ((t :inherit 'bold)))
  ;; :hook
  ;; (latex-mode . auto-fill-mode)
  ;; (LaTeX-mode . auto-fill-mode)
  :config
  (add-to-list 'LaTeX-indent-environment-list '("align*")))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq-local comment-add 0)
            (face-remap-add-relative 'font-lock-type-face '(:inherit default))))

(add-to-list! 'auto-mode-alist
              '("\\.latexmkrc\\'" . perl-mode)
              '("\\.h\\'" . c++-ts-mode)
              '("\\.yql\\'" . sql-mode)
              '("\\.ts\\'" . typescript-ts-mode)
              '("\\.sqlfluff\\'" . conf-mode)
              '("\\.clang-format\\'" . yaml-mode)
              '("\\.tsx\\'" . tsx-ts-mode)
              '("\\.puml\\'" . plantuml-mode)
              '("skhdrc\\'" . conf-mode)
              '("\\(CMakeLists.txt\\|\\.cmake\\)\\'" . cmake-ts-mode)
              '(".arcconfig\\'" . conf-mode)
              '("\\.lua\\'" . lua-ts-mode))

(setq-default css-indent-offset 2)

(use-package ialign
  :commands ialign
  :custom (ialign-initial-repeat t)
  :general
  (nvmap
    "ga" 'ialign))

(add-hook 'window-configuration-change-hook
          (lambda ()
            (when (and (memq major-mode '(text-mode prog-mode))
                       (not (bound-and-true-p visual-fill-column-mode)))
              (set-window-margins nil 2))))

(use-builtin flymake
  :hook
  ((sql-mode) . flymake-mode)
  :commands flymake-mode
  :custom
  (flymake-margin-indicators-string '((error " " compilation-error)
                                      (warning " " compilation-warning)
                                      (note " " compilation-info)))
  (flymake-suppress-zero-counters t)
  (flymake-start-on-flymake-mode t)
  (flymake-no-changes-timeout 1.0)
  (flymake-start-on-save-buffer t)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-wrap-around nil)
  (flymake-indicator-type 'margins)
  :general
  (nvmap
    :prefix "g"
    "n" 'flymake-goto-next-error
    "p" 'flymake-goto-prev-error))

(use-package flymake-sqlfluff
  :ensure (:host github :repo "danilshvalov/flymake-sqlfluff")
  :hook (sql-mode . flymake-sqlfluff-load))

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables '("PATH" "cam" "icg"))
  :preface
  (defun source-file-and-get-envs (filename)
    (let* ((cmd (concat ". " filename "; env"))
           (env-str (shell-command-to-string cmd))
           (env-lines (split-string env-str "\n"))
           (envs (mapcar (lambda (s) (replace-regexp-in-string "=.*$" "" s)) env-lines)))
      (delete "" envs)))

  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs (source-file-and-get-envs "~/.zprofile"))))

(use-builtin dash-modeline
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

(use-builtin help
  :general
  (nvmap "SPC h" `(,(general-simulate-key "C-h") :wk "+help")))

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

  (add-to-list! 'treesit-language-source-alist
                '(markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                                     nil
                                     "tree-sitter-markdown-inline/src"))
                '(markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
                '(gitcommit . ("https://github.com/gbprod/tree-sitter-gitcommit"))
                '(git-rebase . ("https://github.com/the-mikedavis/tree-sitter-git-rebase"))
                '(diff . ("https://github.com/the-mikedavis/tree-sitter-diff"))
                '(sql . ("https://github.com/DerekStride/tree-sitter-sql"))
                '(lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua")))

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
   "C-v" (lambda (file)
           (interactive "f")
           (split-window-right)
           (find-file-other-window file)))
  (:states '(normal visual insert)
           :keymaps 'vertico-map
           "C-v" "C-e C-v"
           "C-s" 'embark-collect
           "C-c" (lambda ()
                   (interactive)
                   (embark-select)
                   (vertico-next)))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; (setq embark-default-action-overrides '((file . consult-fd)))

  (add-to-list
   'display-buffer-alist
   '((lambda (buffer-or-name _)
       (let ((buffer (get-buffer buffer-or-name)))
         (with-current-buffer buffer
           (equal major-mode 'embark-collect-mode))))
     (display-buffer-reuse-window display-buffer-in-direction)
     (direction . bottom)
     (reusable-frames . visible)
     (window-height . 0.2)))

  (setq embark-indicators
        '(embark--vertico-indicator
          embark-minimal-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :config
  (setf (alist-get 'file embark-default-action-overrides
                   nil nil #'equal)
        #'consult-find)
  (setf (alist-get '(file . my-consult-recent-file)
                   embark-default-action-overrides
                   nil nil #'equal)
        #'find-file))

(use-builtin image-mode
  :commands image-mode
  :custom (image-use-external-converter t))

(when (display-graphic-p)
  (set-fringe-style -1))

(use-package pbcopy
  :when (equal system-type 'darwin)
  :init (turn-on-pbcopy))

(setq cc-search-directories '("."
                              "./src/**/*"
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

;; (use-package lua-mode
;;   :mode "\\.lua\\'")

(use-builtin calendar
  :defer t
  :custom
  (calendar-minimum-window-height 10))

(use-builtin treesit
  :custom
  (treesit-font-lock-level 4)
  (major-mode-remap-alist
   '((c++-mode . c++-ts-mode)
     (c-mode . c-ts-mode)
     (python-mode . python-ts-mode)
     (diff-mode . diff-ts-mode)
     ;; (sql-mode . sql-ts-mode)
     ))
  :config
  (defun treesit--install-language-grammar-1
      (out-dir lang url &optional revision source-dir cc c++)
    "Install and compile a tree-sitter language grammar library.

OUT-DIR is the directory to put the compiled library file.  If it
is nil, the \"tree-sitter\" directory under user's Emacs
configuration directory is used (and automatically created if it
does not exist).

For LANG, URL, REVISION, SOURCE-DIR, GRAMMAR-DIR, CC, C++, see
`treesit-language-source-alist'.  If anything goes wrong, this
function signals an error."
    (let* ((lang (symbol-name lang))
           (maybe-repo-dir (expand-file-name url))
           (url-is-dir (file-accessible-directory-p maybe-repo-dir))
           (default-directory (make-temp-file "treesit-workdir" t))
           (workdir (if url-is-dir
                        maybe-repo-dir
                      (expand-file-name "repo")))
           (source-dir (expand-file-name (or source-dir "src") workdir))
           (cc (or cc (seq-find #'executable-find '("cc" "gcc" "c99"))
                   ;; If no C compiler found, just use cc and let
                   ;; `call-process' signal the error.
                   "cc"))
           (c++ (or c++ (seq-find #'executable-find '("c++" "g++"))
                    "c++"))
           (npm (seq-find #'executable-find '("npm")))
           (soext (or (car dynamic-library-suffixes)
                      (signal 'treesit-error '("Emacs cannot figure out the file extension for dynamic libraries for this system, because `dynamic-library-suffixes' is nil"))))
           (out-dir (or (and out-dir (expand-file-name out-dir))
                        (locate-user-emacs-file "tree-sitter")))
           (lib-name (concat "libtree-sitter-" lang soext)))
      (unwind-protect
          (with-temp-buffer
            (if url-is-dir
                (when revision
                  (treesit--git-checkout-branch workdir revision))
              (treesit--git-clone-repo url revision workdir))
            (setq default-directory workdir)
            (message "Generating sources")
            (treesit--call-process-signal npm nil t nil "run" "build")
            ;; We need to go into the source directory because some
            ;; header files use relative path (#include "../xxx").
            ;; cd "${sourcedir}"
            (setq default-directory source-dir)
            (message "Compiling library")
            ;; cc -fPIC -c -I. parser.c
            (treesit--call-process-signal
             cc nil t nil "-fPIC" "-c" "-I." "parser.c")
            ;; cc -fPIC -c -I. scanner.c
            (when (file-exists-p "scanner.c")
              (treesit--call-process-signal
               cc nil t nil "-fPIC" "-c" "-I." "scanner.c"))
            ;; c++ -fPIC -I. -c scanner.cc
            (when (file-exists-p "scanner.cc")
              (treesit--call-process-signal
               c++ nil t nil "-fPIC" "-c" "-I." "scanner.cc"))
            ;; cc/c++ -fPIC -shared *.o -o "libtree-sitter-${lang}.${soext}"
            (apply #'treesit--call-process-signal
                   (if (file-exists-p "scanner.cc") c++ cc)
                   nil t nil
                   (if (eq system-type 'cygwin)
                       `("-shared" "-Wl,-dynamicbase"
                         ,@(directory-files
                            default-directory nil
                            (rx bos (+ anychar) ".o" eos))
                         "-o" ,lib-name)
                     `("-fPIC" "-shared"
                       ,@(directory-files
                          default-directory nil
                          (rx bos (+ anychar) ".o" eos))
                       "-o" ,lib-name)))
            ;; Copy out.
            (unless (file-exists-p out-dir)
              (make-directory out-dir t))
            (let* ((library-fname (expand-file-name lib-name out-dir))
                   (old-fname (concat library-fname ".old")))
              ;; Rename the existing shared library, if any, then
              ;; install the new one, and try deleting the old one.
              ;; This is for Windows systems, where we cannot simply
              ;; overwrite a DLL that is being used.
              (if (file-exists-p library-fname)
                  (rename-file library-fname old-fname t))
              (copy-file lib-name (file-name-as-directory out-dir) t t)
              ;; Ignore errors, in case the old version is still used.
              (ignore-errors (delete-file old-fname)))
            (message "Library installed to %s/%s" out-dir lib-name))
        ;; Remove workdir if it's not a repo owned by user and we
        ;; managed to create it in the first place.
        (when (and (not url-is-dir) (file-exists-p workdir))
          (delete-directory workdir t))))))

                                        ; (use-package rainbow-mode
                                        ;   :general
                                        ;   (nvmap
                                        ;     :prefix "SPC t"
                                        ;     "r" 'rainbow-mode))

(use-builtin eldoc
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-builtin vc
  :defer t
  :custom
  (vc-handled-backends nil))

(use-package powershell
  :defer t
  :custom
  (powershell-indent 2))

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
  :defer t
  :custom
  (plantuml-indent-level 4))

(use-package php-mode
  :defer t)

(defun set-tab-name ()
  (interactive)
  ;; (tab-bar-rename-tab (or (get-pwd) (number-to-string (random))))
  (tab-bar-rename-tab (number-to-string (random)))
  )

(set-tab-name)
;; (add-hook 'tab-bar-mode-hook (lambda () (interactive) (set-tab-name)))
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
    (cdr (assoc 'name (cdr (tab-bar--current-tab-find nil nil))))
    )
  :config
  ;; (set-tab-name)
  ;; (run-with-idle-timer 2 nil 'set-tab-name)

  (advice-add
   'tab-bar-new-tab
   :around
   (lambda (fun &rest args)
     (apply fun args)
     (set-tab-name)
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

(use-package dockerfile-mode
  :defer t)

(use-package nginx-mode
  :defer t)

(defmacro markdown-ts-capture (parser-language parser-mode)
  `(lambda (beg end)
     (let* ((parser (treesit-parser-create 'markdown))
            (root (treesit-parser-root-node parser))
            (query '((fenced_code_block
                      (info_string) @lang
                      (code_fence_content) @content)))
            (captures (treesit-query-capture root query beg end))

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
  :defer t
  :general
  (nmap
    :keymaps 'override
    :prefix "SPC c"
    "d" (lambda ()
          (interactive)
          (cd default-directory)
          (message "Directory: %s" default-directory))
    "D" 'cd)
  :config
  (advice-add
   'cd
   :after
   (lambda (&rest args)
     (my-set-current-directory (car args)))))

(use-package web-mode
  :defer t
  :custom
  (web-mode-enable-auto-expanding t)
  (web-mode-enable-auto-closing t)
  :config
  (add-hook 'mhtml-mode-hook 'web-mode))

(advice-add 'indent-for-tab-command
            :around
            (lambda (fun &rest args)
              (if (eq evil-state 'insert)
                  (tab-to-tab-stop)
                (apply fun args))))

(define-advice server-eval-and-print (:filter-args (args) no-print)
  (list (car args) nil))

(use-builtin obsidian
  :mode "\\.md\\'"
  :custom
  (obsidian-workspaces '((notes . "~/obsidian")))
  (obsidian-daily-note-directory "ежедневник")
  :general
  (nvmap
    :keymaps 'override
    :prefix "SPC o"
    "t" (lambda ()
          (interactive)
          (find-file "~/obsidian/список-задач.md"))
    "T" 'obsidian-today
    "o" 'obsidian-open
    "i" 'obsidian-insert-tag
    "n" 'obsidian-new
    "f" (lambda ()
          (interactive)
          (consult-find "~/obsidian"))
    "g" (lambda ()
          (interactive)
          (consult-ripgrep "~/obsidian"))))

(use-package edit-indirect
  :commands markdown-ts-mode)

(use-builtin markdown-ts-mode
  :ensure (markdown-ts-mode :repo "~/projects/markdown-ts-mode" :main "markdown-ts-mode.el")
  :mode "\\.md\\'"
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-list-item-bullets '("—"))
  (markdown-ts-code-language-modes
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
     ("screen" . shell-script-mode)
     ("shell" . sh-mode)
     ("bash" . sh-mode)))
  :hook (markdown-ts-mode . (lambda ()
                              (setq-local adaptive-fill-regexp "[-–!|#%;>·•‣⁃◦ 	]*")))
  :general
  (general-define-key
   :keymaps 'markdown-ts-mode-map
   "C-c C-c" 'markdown-ts-toggle-checkbox)
  :config
  (font-lock-add-keywords 'markdown-ts-mode
                          '(("\\(^\\|[[:space:]]+\\)\\(#[[:alnum:]-_/]+\\)" 2 'obsidian-tag prepend)) t))

(use-package evil-anzu
  :demand t
  :after evil
  :custom
  (anzu-cons-mode-line-p nil)
  :config
  (global-anzu-mode t)

  (defun my/anzu-update-func (here total)
    (when anzu--state
      (let ((status (cl-case anzu--state
                      (search (format "%d/%d" here total))
                      (replace-query (format "%d Replaces" total))
                      (replace (format "%d/%d" here total)))))
        (propertize status 'face 'anzu-mode-line))))

  (custom-set-variables
   '(anzu-mode-line-update-function #'my/anzu-update-func)))

(add-hook 'server-after-make-frame-hook
          (lambda ()
            (set-tab-name)
            (cd (process-get (frame-parameter nil 'client) 'server-client-directory))
            (dired (project-root-current))))

(defun yank--file-name (format-fn &rest args)
  (let ((filename (apply format-fn (buffer-file-name) args)))
    (kill-new filename)
    (message "Copied: %s" filename)))

(nvmap
  :prefix "SPC y"
  "" '(nil :wk "yank")
  "a" '((lambda()
          (require 'arc)
          (interactive)
          (yank--file-name (lambda (f)
                             (arc-make-link nil))))
        :wk "Arcadia URL")
  "A" '((lambda ()
          (require 'arc)
          (interactive)
          (yank--file-name (lambda (f)
                             (arc-make-link t))))
        :wk "Arcadia URL with line number")
  "f" '((lambda ()
          (interactive)
          (yank--file-name 'file-relative-name))
        :wk "Filename")
  "b" '((lambda ()
          (interactive)
          (yank--file-name 'file-name-base))
        :wk "Filename base")
  "p" '((lambda ()
          (interactive)
          (yank--file-name (lambda (f) f)) :wk "Path")))

(use-package indent-bars
  :ensure (indent-bars :host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-color-by-depth nil)
  (indent-bars-color '(shadow :face-bg nil :blend 0.5))
  (indent-bars-prefer-character t)
  :hook ((prog-mode text-mode) . indent-bars-mode))

;; (global-auto-revert-mode)

(defun my-switch-project (dir)
  "\"Switch\" to another project by running an Emacs command.
The available commands are presented as a dispatch menu
made from `project-switch-commands'.

When called in a program, it will use the project corresponding
to directory DIR."
  (interactive (list (project-prompt-project-dir)))
  (cd dir)
  (dired dir))

(use-package git-commit-ts-mode
  :ensure (git-commit-ts-mode :repo "~/projects/git-commit-ts-mode")
  ;; :ensure (git-commit-ts-mode :host github :repo "danilshvalov/git-commit-ts-mode")
  :mode "\\COMMIT_EDITMSG\\'"
  :config
  (when (daemonp)
    (nmap git-commit-ts-mode-map
      "C-c C-c" 'server-edit)))

(use-package git-rebase-ts-mode
  :ensure nil
  :mode "arc-rebase-todo\\'"
  ;; :ensure (git-commit-ts-mode :host github :repo "danilshvalov/git-commit-ts-mode")
  :config
  (when (daemonp)
    (nmap git-commit-ts-mode-map
      "C-c C-c" 'server-edit)))

(use-builtin diff-ts-mode
  :mode "\\.diff\\'"
  ;; :elpaca (diff-ts-mode :repo "~/projects/diff-ts-mode")
  :load-path "~/projects/diff-ts-mode"
  ;; :elpaca (git-commit-ts-mode :host github
  ;;                             :repo "danilshvalov/git-commit-ts-mode")
  )


(use-builtin arc
  :general
  (nvmap
    :keymaps 'override
    :prefix "SPC a"
    "c" 'arc-branch-checkout
    "t" 'arc-insert-ticket
    "f" 'arc-find-file
    "d" 'arc-diff-trunk
    "P" 'arc-pull
    "i" 'arc-info)
  :config
  (advice-add 'arc--call-process :around #'execute-at-project-root))

(defun find-file-wrapper (fun &rest args)
  (let ((args (push (substitute-in-file-name (string-trim (car args)))
                    (cdr args))))
    (apply fun args)))

(advice-add 'find-file :around #'find-file-wrapper)

(defun markdown-gx-wrapper ()
  (interactive)
  (let ((url (markdown-link-url)))
    (if url
        (markdown-follow-link-at-point)
      (browse-url-at-point))))

(nvmap "gx" 'markdown-gx-wrapper)

(use-package evil-lion
  :after evil
  :custom (evil-lion-squeeze-spaces nil)
  :config (evil-lion-mode t))

(use-package protobuf-mode
  :mode "\\.proto\\'")

;; (add-hook 'after-change-major-mode-hook
;;           (lambda ()
;;             (modify-syntax-entry ?_ "w")))

(add-hook 'python-ts-mode-hook (lambda () (setq-local fill-column 120)))


(defcustom treesit-injections-code-lang-modes
  '(("ocaml" . tuareg-mode) ("elisp" . emacs-lisp-mode) ("ditaa" . artist-mode)
    ("asymptote" . asy-mode) ("dot" . fundamental-mode) ("sqlite" . sql-mode)
    ("calc" . fundamental-mode) ("C" . c-mode) ("cpp" . c++-mode)
    ("C++" . c++-mode) ("screen" . shell-script-mode) ("shell" . sh-mode)
    ("bash" . sh-mode))
  "Alist mapping languages to their major mode.
The key is the language name, the value is the major mode.  For
many languages this is simple, but for language where this is not
the case, this variable provides a way to simplify things on the
user side.  For example, there is no ocaml-mode in Emacs, but the
mode to use is `tuareg-mode'."
  ;; :group 'markdown
  :type '(repeat
          (cons
           (string "Language name")
           (symbol "Major mode"))))

(defun treesit-injections--lang-mode-predicate (mode)
  (and mode
       (fboundp mode)
       (or
        ;; https://github.com/jrblevin/markdown-ts-mode/issues/787
        ;; major-mode-remap-alist was introduced at Emacs 29.1
        (cl-loop for pair in (bound-and-true-p major-mode-remap-alist)
                 for func = (cdr pair)
                 thereis (and (atom func) (eq mode func)))
        ;; https://github.com/jrblevin/markdown-ts-mode/issues/761
        (cl-loop for pair in auto-mode-alist
                 for func = (cdr pair)
                 thereis (and (atom func) (eq mode func))))))


(defun treesit-injections-get-lang-mode (lang)
  "Return major mode that should be used for LANG.
LANG is a string, and the returned major mode is a symbol."
  (cl-find-if
   #'treesit-injections--lang-mode-predicate
   (nconc (list (cdr (assoc lang markdown-code-lang-modes))
                (cdr (assoc (downcase lang) markdown-code-lang-modes)))
          (and (fboundp 'treesit-language-available-p)
               (list (and (treesit-language-available-p (intern lang))
                          (intern (concat lang "-ts-mode")))
                     (and (treesit-language-available-p (intern (downcase lang)))
                          (intern (concat (downcase lang) "-ts-mode")))))
          (list
           (intern (concat lang "-mode"))
           (intern (concat (downcase lang) "-mode")))
          (list 'fundamental-mode))))

(defun treesit-injections--fontify-code-block-natively (lang start end)
  (interactive)
  (let ((lang-mode (if lang (treesit-injections-get-lang-mode lang)
                     fundamental-mode)))
    (when (fboundp lang-mode)
      (let ((string (buffer-substring-no-properties start end))
            (modified (buffer-modified-p))
            (has-font-lock-mode font-lock-mode)
            (buffer (current-buffer)) pos next)
        (remove-text-properties start end '(face nil))
        (with-current-buffer
            (get-buffer-create
             (concat " treesit-injections-fontification:" (symbol-name lang-mode)))
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
                (put-text-property
                 (+ start (1- pos))
                 (1- (+ start next))
                 'face
                 val buffer)))
            (setq pos next)))
        (set-buffer-modified-p modified)))))



;; (defun treesit-tmp-fontify-code-block (node)
;;   (when-let* ((query '((fenced_code_block
;;                    (info_string
;;                     (language) @language)
;;                    (code_fence_content) @content)))
;;          (captures (treesit-query-capture node query))
;;          (language-node (cdr (assoc 'language captures)))
;;          (language (treesit-node-text language-node))
;;          (language (intern language))
;;          (parser-exists (treesit-ready-p language))
;;          (parser (treesit-parser-create language))
;;          (content-node (cdr (assoc 'content captures)))
;;          (range (cons (treesit-node-start content-node)
;;                       (treesit-node-end content-node))))
;;     (cons parser range)))

;; (defun markdown--code-blocks-parsers-ranges (start end)
;;   (let* ((parser (treesit-parser-create 'markdown))
;;          (root (treesit-parser-root-node parser))
;;          (query '((fenced_code_block) @capture))
;;          (captures (treesit-query-capture root query start end))
;;          (parser-ranges))
;;     (dolist (capture captures)
;;       (when-let ((parser-range (treesit-tmp-fontify-code-block (cdr capture))))
;;         (let* ((parser (car parser-range))
;;                (range (cdr parser-range))
;;                (previous-ranges (cdr (assoc parser parser-ranges)))
;;                (new-ranges (push range previous-ranges)))
;;           (setf (alist-get parser parser-ranges) new-ranges))))
;;     (cl-loop for (parser . ranges) in parser-ranges
;;              collect (cons parser (reverse ranges)))))

;; (defun markdown-fontify-code-blocks (start end)
;;   (cl-loop for (parser . ranges) in (markdown--code-blocks-parsers-ranges start end) do
;;            (treesit-parser-set-included-ranges
;;             parser
;;             (treesit--clip-ranges
;;              (treesit--merge-ranges
;;               (treesit-parser-included-ranges parser)
;;               ranges start end)
;;              (point-min) (point-max)))))


;; (add-hook 'markdown-ts-mode-hook
;;           (lambda ()
;;             (setq treesit-range-settings
;;                   (treesit-range-rules
;;                    'markdown-fontify-code-blocks))))

;; (defun markdown-language-at-point (pos)
;;   (let* ((parser (treesit-parser-create 'markdown))
;;          (root (treesit-parser-root-node parser))
;;          (query '((fenced_code_block
;;                    (info_string
;;                     (language) @capture))))
;;          (captures (treesit-query-capture root query pos pos))
;;          (node (cdr (assoc 'capture captures))))
;;     (if node (intern (treesit-node-text node))
;;       'markdown)))

;; (advice-add
;;  'treesit-font-lock-fontify-region
;;  :after
;;  (lambda (start end &optional loudly)
;;    (dolist (parser (treesit-parser-list))
;;      (let ((language (symbol-name
;;                       (treesit-parser-language parser))))
;;        (dolist (range (treesit-parser-included-ranges parser))
;;          (let ((range-start (car range))
;;                (range-end (cdr range)))
;;            (unless (or (< end range-start) (< range-end start))
;;              (treesit-injections--fontify-code-block-natively language range-start range-end))))))
;;    (add-text-properties
;;     start end
;;     '(font-lock-fontified t fontified t font-lock-multiline t))))

;; (defun markdown-ts-mode--parser-notifier (ranges parser)
;;   (message "Data: %s %s" parser ranges))

(use-builtin which-key
  :custom
  (which-key-separator " → " )
  (which-key-min-display-lines 10)
  (which-key-max-description-length 50)
  (which-key-add-column-padding 2)
  :init
  (which-key-mode))

(use-package visual-fill-column
  :custom
  (visual-fill-column-center-text t)
  :hook ((markdown-ts-mode latex-mode LaTeX-mode) . visual-fill-column-mode))

(use-package eglot-booster
  :ensure (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(use-package hydra
  :general
  (nvmap
    :prefix "C-w"
    "C-w" #'hydra-window/body)
  :preface
  (require 'windmove)

  (defun hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))

  :config
  (defhydra hydra-window (:color red :hint nil)
    "
Move: _h_: left  _j_: down  _k_: up  _l_: right"
    ("h" hydra-move-splitter-left)
    ("j" hydra-move-splitter-down)
    ("k" hydra-move-splitter-up)
    ("l" hydra-move-splitter-right)))

;; (use-package sql-ts-mode
;;   :ensure (sql-ts-mode :host github :repo "nverno/sql-ts-mode")
;;   :mode "\\.sql\\'"
;;   :preface
;;   (defun my-markdown-ts-mode-faces ()
;;     "Set background color for use only in emacs lisp modes."
;;     (interactive)
;;     (face-remap-add-relative 'font-lock-variable-name-face `(:foreground ,(doom-color 'yellow))))
;;   :config
;;   (add-hook 'sql-ts-mode-hook #'my-markdown-ts-mode-faces))

(defvar yaml-indent-offset 4)

(use-builtin yaml-ts-mode
  :mode "\\.ya?ml\\'")

(setq word-wrap-by-category t)
;; Add the | (= line-breakable) category to the - char.
(modify-category-entry ?- ?| (standard-category-table))
(modify-category-entry ?_ ?| (standard-category-table))
(modify-category-entry ?/ ?| (standard-category-table))

(add-to-list 'safe-local-variable-values '(apheleia-formatter . (taxi-format)))
(add-to-list! 'safe-local-variable-directories
              (expand-file-name "~")
              (expand-file-name "~/arcadia/taxi"))
