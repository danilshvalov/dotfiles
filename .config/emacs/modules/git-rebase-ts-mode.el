;;; git-rebase-ts-mode.el --- Tree-sitter support for Git rebase files -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Daniil Shvalov

;; Author: Daniil Shvalov <daniil.shvalov@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Keywords: tree-sitter, git, faces
;; Homepage: https://github.com/danilshvalov/git-rebase-ts-mode

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A tree-sitter based major mode for editing Git rebase files in GNU Emacs

;;; Code:
(require 'treesit)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-child "treesit.c")

(defgroup git-rebase-ts nil
  "Git rebase commands."
  :group 'extensions)

(defgroup git-rebase-ts-faces nil
  "Faces used by git rebase."
  :group 'git-rebase-ts)

(defface git-rebase-ts-comment-face '((t :inherit font-lock-comment-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-comment-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-face '((t :inherit font-lock-keyword-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-commit-face '((t :inherit font-lock-keyword-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-label-face '((t :inherit font-lock-function-call-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-fixup-face '((t :inherit font-lock-warning-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-merge-face '((t :inherit git-rebase-ts-command-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-label-face '((t :inherit font-lock-constant-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-hash-face '((t :inherit font-lock-builtin-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-message-face '((t :inherit font-lock-string-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-option-face '((t :inherit font-lock-operator-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-pick-face '((t :inherit git-rebase-ts-command-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-reword-face '((t :inherit git-rebase-ts-command-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-edit-face '((t :inherit git-rebase-ts-command-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-squash-face '((t :inherit git-rebase-ts-command-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-label-face '((t :inherit git-rebase-ts-command-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-reset-face '((t :inherit git-rebase-ts-command-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-drop-face '((t :inherit git-rebase-ts-command-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-exec-face '((t :inherit git-rebase-ts-command-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-break-face '((t :inherit git-rebase-ts-command-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-fixup-face '((t :inherit git-rebase-ts-command-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defface git-rebase-ts-command-merge-face '((t :inherit git-rebase-ts-command-face))
  "Face used for comments. Example:

   some commit message

   # this is my comment
   └──────────────────┘

The underlined text will be highlighted using `git-rebase-ts-command-label-face'."
  :group 'git-rebase-ts-faces)

(defcustom git-rebase-ts-command-commit-regexp
  "^\\(p\\|pick\\|r\\|reword\\|e\\|edit\\|s\\|squash\\|d\\|drop\\)$"
  "The maximum allowed commit message size. If the specified limit is exceeded,
the rest of the message will be highlighted using
`git-commit-ts-overflow-face'."
  :type 'string
  :safe 'stringp
  :group 'git-rebase-ts)

(defcustom git-rebase-ts-command-label-regexp
  "^\\(l\\|label\\|t\\|reset\\)$"
  "The maximum allowed commit message size. If the specified limit is exceeded,
the rest of the message will be highlighted using
`git-commit-ts-overflow-face'."
  :type 'string
  :safe 'stringp
  :group 'git-rebase-ts)

(defcustom git-rebase-ts-command-execute-regexp
  "^\\(x\\|exec\\|b\\|break\\)$"
  "The maximum allowed commit message size. If the specified limit is exceeded,
the rest of the message will be highlighted using
`git-commit-ts-overflow-face'."
  :type 'string
  :safe 'stringp
  :group 'git-rebase-ts)

(defcustom git-rebase-ts-command-fixup-regexp
  "^\\(f\\|fixup\\)$"
  "The maximum allowed commit message size. If the specified limit is exceeded,
the rest of the message will be highlighted using
`git-commit-ts-overflow-face'."
  :type 'string
  :safe 'stringp
  :group 'git-rebase-ts)

(defcustom git-rebase-ts-command-merge-regexp
  "^\\(m\\|merge\\)$"
  "The maximum allowed commit message size. If the specified limit is exceeded,
the rest of the message will be highlighted using
`git-commit-ts-overflow-face'."
  :type 'string
  :safe 'stringp
  :group 'git-rebase-ts)

(defvar git-rebase-ts-font-lock-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'git-rebase
   '((comment) @git-rebase-ts-comment-face)

   :feature 'command
   :language 'git-rebase
   '(((command) @git-rebase-ts-command-pick-face
      (label) @git-rebase-ts-hash-face
      (message) :? @git-rebase-ts-comment-face
      (:match "^\\(p\\|pick\\)$"
              @git-rebase-ts-command-pick-face)))

   :feature 'command
   :language 'git-rebase
   '(((command) @git-rebase-ts-command-reword-face
      (label) @git-rebase-ts-hash-face
      (message) :? @git-rebase-ts-comment-face
      (:match "^\\(r\\|reword\\)$"
              @git-rebase-ts-command-reword-face)))

   :feature 'command
   :language 'git-rebase
   '(((command) @git-rebase-ts-command-edit-face
      (label) @git-rebase-ts-hash-face
      (message) :? @git-rebase-ts-comment-face
      (:match "^\\(e\\|edit\\)$"
              @git-rebase-ts-command-edit-face)))

   :feature 'command
   :language 'git-rebase
   '(((command) @git-rebase-ts-command-squash-face
      (label) @git-rebase-ts-hash-face
      (message) :? @git-rebase-ts-comment-face
      (:match "^\\(s\\|squash\\)$"
              @git-rebase-ts-command-squash-face)))

   :feature 'command
   :language 'git-rebase
   '(((command) @git-rebase-ts-command-drop-face
      (label) @git-rebase-ts-hash-face
      (message) :? @git-rebase-ts-comment-face
      (:match "^\\(d\\|drop\\)$"
              @git-rebase-ts-command-drop-face)))

   :feature 'command
   :language 'git-rebase
   `(((command) @git-rebase-ts-command-label-face
      (label) @git-rebase-ts-label-face
      (message) :? @git-rebase-ts-comment-face
      (:match "^\\(l\\|label\\)$"
              @git-rebase-ts-command-label-face)))

   :feature 'command
   :language 'git-rebase
   `(((command) @git-rebase-ts-command-reset-face
      (label) @git-rebase-ts-reset-face
      (message) :? @git-rebase-ts-comment-face
      (:match "^\\(r\\|reset\\)$"
              @git-rebase-ts-command-reset-face)))

   :feature 'command
   :language 'git-rebase
   `(((command) @git-rebase-ts-command-exec-face
      (:match "^\\(x\\|exec\\)$"
              @git-rebase-ts-command-exec-face)))

   :feature 'command
   :language 'git-rebase
   `(((command) @git-rebase-ts-command-break-face
      (:match "^\\(b\\|break\\)$"
              @git-rebase-ts-command-break-face)))

   :feature 'command
   :language 'git-rebase
   `(((command) @git-rebase-ts-command-fixup-face
      (label) @git-rebase-ts-hash-face
      (message) :? @git-rebase-ts-comment-face
      (:match "^\\(f\\|fixup\\)$"
              @git-rebase-ts-command-fixup-face)))

   :feature 'command
   :language 'git-rebase
   `(((command) @git-rebase-ts-command-merge-face
      (label) @git-rebase-ts-hash-face
      (label) @git-rebase-ts-label-face
      (message) :? @git-rebase-ts-message-face
      (:match "^\\(m\\|merge\\)$"
              @git-rebase-ts-command-merge-face)))

   :feature 'option
   :language 'git-rebase
   `((option) @git-rebase-ts-option-face))
  "Tree-sitter font-lock settings for `git-rebase-ts-mode'.")

;;;###autoload
(define-derived-mode git-rebase-ts-mode prog-mode "Git rebase"
  (when (treesit-ready-p 'git-rebase)
    (treesit-parser-create 'git-rebase)
    (setq-local treesit-font-lock-settings git-rebase-ts-font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (command)
                  (option)))
    (treesit-major-mode-setup)
    (add-to-list 'auto-mode-alist '("git-rebase-todo\\'" . git-rebase-ts-mode))))

(provide 'git-rebase-ts-mode)

;;; git-rebase-ts-mode.el ends here
