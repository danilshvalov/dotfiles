;;; dash-modeline.el -*- lexical-binding: t -*-

;;; Code:

(defgroup dash nil
  "dash"
  :group 'convenience)

(defgroup dash-modeline nil
  "dash modeline"
  :group 'dash)

(defcustom dash-modeline-padding '(0.1 . 0.1)
  "Default vertical space adjustment (in fraction of character height)"
  :type '(cons (float :tag "Top spacing")
               (float :tag "Bottom spacing"))
  :group 'dash-modeline)

(defcustom dash-modeline-position #'dash-modeline-header
  "Default position for the dash modeline"

  :type '(choice (const :tag "Top"    dash-modeline-header)
                 (const :tag "Bottom" dash-modeline-footer))
  :group 'dash-modeline)

(defface dash-modeline-default
  `((t (:foreground ,(face-foreground 'default)
        :background ,(face-background 'header-line nil t))))
    "Default face for modeline")

(defface dash-modeline-status
  `((t (:foreground ,(face-background 'default)
        :background ,(face-foreground 'shadow nil t)
        :inherit bold)))
  "Face for line status")

(defface dash-modeline-status-modified
  `((t (:inherit dash-modeline-status)))
  "Face for line status")

(defun dash-modeline--stroke-width (face)
  "Extract the line width of the box for the given FACE."

  (let* ((box (face-attribute face ':box nil 'default))
         (width (plist-get box ':line-width)))
      (cond ((integerp width) width)
            ((consp width) (car width))
            (t 0))))

;; Dash line faces
(defcustom dash-modeline-faces
  `((header    . (dash-modeline-default))
    (footer    . (dash-modeline-default))
    (position  . (dash-modeline-status))
    (status-RW . (dash-modeline-status))
    (status-RO . (dash-modeline-status))
    (status-** . (dash-modeline-status-modified))
    (name      . (bold))
    (primary   . ())
    (success   . (success))
    (warning   . (warning))
    (error     . (error))
    (secondary . (,(when (facep 'nano-faded) 'nano-faded))))
  "Dash line faces.

Each face defined here is used by the modeline depending on the current state (active / inactive). It is ok to define a face for a single state. In such case, the alternative state will use defaults."
  :type '(alist :key-type (symbol :tag "Face")
                :value-type (repeat :tag "inherits" face)))

(defface dash-modeline--empty-face
  `((t (:foreground ,(face-foreground 'default))))
  "Empty face for resetting mode-line / header-line."
  :group nil)

(defvar dash-modeline--selected-window nil
  "Selected window before mode-line was activated.")

(defun dash-modeline--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (setq dash-modeline--selected-window (selected-window)))

(defun dash-modeline--base-face (face-prefix)
  "Return the face for FACE-PREFIX according to current active state."

  (let* ((window (get-buffer-window (current-buffer)))
         (state (intern (symbol-name face-prefix)))
         (face (cdr (assoc state dash-modeline-faces))))
    face))

(defun dash-modeline-face (&optional face-prefix)
  "Return the face for FACE-PREFIX according to current active state and
make it inherit the base face."

  (let* ((window (get-buffer-window (current-buffer)))
         (state (intern (symbol-name face-prefix)))
         (face (cdr (assoc state dash-modeline-faces)))
         (face (if (boundp 'dash-modeline-base-face)
                           (push dash-modeline-base-face face)
                   face))
         (face (reverse face)))
    `(:inherit ,face)))

(defun dash-modeline--make (left right face-prefix)
  "Build a dynamic mode/header line made of LEFT and RIGHT part,
using the given FACE-PREFIX as the default."

  `(:eval
    (let* ((dash-modeline-base-face (dash-modeline--base-face ',face-prefix))
           (left (mapconcat
                  (lambda (element)
                    (if (stringp element)
                        (propertize element 'face dash-modeline-base-face)
                      (apply (car element) (cdr element))))
                  ',left))
           (right (mapconcat
                   (lambda (element)
                     (if (stringp element)
                         (propertize element 'face dash-modeline-base-face)
                       (apply (car element) (cdr element))))
                   ',right))
           (width (window-width))
           (left-max-size (- width (length right) 2))
           (left (if (> (length left) left-max-size)
                     (concat (truncate-string-to-width left left-max-size)
                             (propertize "…" 'face `(:inherit  ,dash-modeline-base-face)))
                   left)))
      (concat left
              (propertize " "
                          'face `(:inherit ,dash-modeline-base-face)
                          ;; 'display `(space :align-to (+ (- right ,(length right)) right-margin 1)))
                          'display `(space :align-to (- (+ right right-fringe right-margin) ,(length right))))
              right))))



(defun dash-modeline--stroke-color (face)
  "Extract the line color of the box for the given FACE."

  (let* ((box (face-attribute face ':box))
         (color (plist-get box ':color)))
    (cond ((stringp color) color)
          (t (face-foreground face nil 'default)))))

(defun dash-modeline-header (left &optional right default)
  "Install a header line made of LEFT and RIGHT parts. Line can be
made DEFAULT."

  (require 'tooltip)

  (if default
      (setq-default header-line-format (dash-modeline--make left right 'header))
    (setq-local header-line-format (dash-modeline--make left right 'header)))
  (face-remap-set-base 'header-line 'dash-modeline--empty-face)
  (add-hook 'post-command-hook #'dash-modeline--update-selected-window))

(defun dash-modeline-footer (left &optional right default)
  "Install a footer line made of LEFT and RIGHT parts. Line can be
made DEFAULT."

  (if default
      (setq-default mode-line-format (dash-modeline--make left right 'header))
    (setq-local mode-line-format (dash-modeline--make left right 'header)))
  (face-remap-set-base 'mode-line 'dash-modeline--empty-face)
  (face-remap-set-base 'mode-line-inactive 'dash-modeline-empty-face)
  (add-hook 'post-command-hook #'dash-modeline--update-selected-window))

(defun dash-modeline-buffer-name (&optional name)
  "Buffer name"

  (propertize
   (cond (name name)
         ((buffer-narrowed-p) (format"%s [narrow]" (buffer-name)))
         (t (buffer-name)))
   'face (dash-modeline-face 'name)))

(defun dash-modeline-buffer-status (&optional status padding)
  "Generic prefix to indicate buffer STATUS with vertical PADDING (top . bottom)"

  (let* ((padding (or padding dash-modeline-padding))
         (top (propertize " " 'display `(raise ,(car padding))))
         (bot (propertize " " 'display `(raise ,(- (cdr padding))))))
    (cond (buffer-read-only
           (propertize (concat top (or status "RO") bot)
                       'face (dash-modeline-face 'status-RO)))
          ((buffer-modified-p)
           (propertize (concat top (or status "**") bot)
                       'face (dash-modeline-face 'status-**)))
          (t
           (propertize (concat top (or status "RW") bot)
                       'face (dash-modeline-face 'status-RW))))))

(defun dash-modeline-file-size ()
  "File size in human readable format"

  (if-let* ((file-name (buffer-file-name))
            (file-attributes (file-attributes file-name))
            (file-size (file-attribute-size file-attributes))
            (file-size (file-size-human-readable file-size)))
      (propertize (format "(%s)" file-size)
                  'face (dash-modeline-face 'primary))
    ""))

(defun dash-modeline-cursor-position (&optional format)
  "Cursor position using given FORMAT."

  (let ((format (or format " %l:%c ")))
    (propertize (format-mode-line format)
                'face (dash-modeline-face (if (buffer-modified-p)
                                              'status-**
                                            'status-RW)))))

(defun dash-modeline-buffer-line-count ()
  "Buffer total number of lines"

  (save-excursion
    (goto-char (point-max))
    (propertize
     (format-mode-line "%lL")
     'face (dash-modeline-face 'primary))))

(defun dash-modeline-datetime (&optional format)
  "Cursor position using given FORMAT."

  (let ((format (or format "%A %d.%m %H:%M")))
    (propertize (format-time-string format)
                'face (dash-modeline-face 'primary))))

(defun dash-modeline-window-dedicated (&optional symbol)
  "Pin symbol when window is dedicated"

  (propertize (if (window-dedicated-p) (or symbol " ") "")
              'face (dash-modeline-face 'secondary)))

(defun dash-modeline--git-branch ()
  (let ((args '("symbolic-ref" "HEAD" "--short")))
    (condition-case nil
        (with-temp-buffer
          (apply #'process-file "git" nil (list t nil) nil args)
          (unless (bobp)
            (goto-char (point-min))
            (buffer-substring-no-properties (point) (line-end-position))))
      (error nil))))

(defun dash-modeline-git-info (&optional symbol)
  "Git information as (branch, file status)"
  (when-let ((branch (dash-modeline--git-branch)))
    (propertize (concat (or symbol " ") branch)
                    'face (dash-modeline-face 'primary))))

(defun dash-modeline--flymake-counter (type icon face)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0)
         (face (dash-modeline-face face)))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (> count 0)
      (propertize
       (concat
        icon
        (format "%d" count))
       'face face))))

(defun dash-modeline-flymake ()
  "Flymake diagnostics"
  (when (bound-and-true-p flymake-mode)
      (let ((notes (dash-modeline--flymake-counter :note " " 'success))
            (warnings (dash-modeline--flymake-counter :warning " " 'warning))
            (errors (dash-modeline--flymake-counter :error " " 'error)))
        (string-join (delq nil (list notes warnings errors)) " "))))

(defun dash-modeline--flycheck-count-errors ()
  "Count the number of ERRORS, grouped by level.

Return an alist, where each ITEM is a cons cell whose `car' is an
error level, and whose `cdr' is the number of errors of that
level."
  (let ((info 0) (warning 0) (error 0))
    (mapc
     (lambda (item)
       (let ((count (cdr item)))
         (pcase (flycheck-error-level-compilation-level (car item))
           (0 (cl-incf info count))
           (1 (cl-incf warning count))
           (2 (cl-incf error count)))))
     (flycheck-count-errors flycheck-current-errors))
    `((info . ,info) (warning . ,warning) (error . ,error))))

(defun dash-modeline--flycheck-icon (count icon face)
  (when (> count 0)
    (propertize
      (concat icon (format "%d" count))
      'face face)))

(defun dash-modeline-flycheck ()
  "Flycheck diagnostics"
  (when (bound-and-true-p flycheck-mode)
    (let-alist (dash-modeline--flycheck-count-errors)
      (let ((notes (dash-modeline--flycheck-icon .info " " 'success))
              (warnings (dash-modeline--flycheck-icon .warning " " 'warning))
              (errors (dash-modeline--flycheck-icon .error " " 'error)))
             (string-join (delq nil (list notes warnings errors)) " ")))))

(defun dash-modeline-pdf-page ()
  "PDF view mode page number / page total"

  (let ((page-current (image-mode-window-get 'page))
        (page-total (pdf-cache-number-of-pages)))
    (propertize (format "%d/%d " page-current page-total)
                'face (dash-modeline-face 'secondary))))

(defun dash-modeline-term-shell-name ()
  "Term shell name"

  (propertize shell-file-name
              'face (dash-modeline-face 'name)))

(defun dash-modeline-default-directory (&optional max-length)
  "Term current directory"

  (let* ((max-length (or max-length 32))
         (dir default-directory)
         (path (reverse (split-string (abbreviate-file-name dir) "/")))
         (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 0)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    (propertize output 'face (dash-modeline-face 'secondary))))

(defsubst dash-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(defun dash-modeline-selection-info ()
  "Information about the current selection.

Such as how many characters and lines are selected, or the NxM dimensions of a
block selection."
  (when (or mark-active
            (and (bound-and-true-p evil-local-mode)
                 (eq evil-state 'visual)))
    (cl-destructuring-bind (beg . end)
      (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
          (cons evil-visual-beginning evil-visual-end)
        (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (cond ((or (bound-and-true-p rectangle-mark-mode)
                    (and (bound-and-true-p evil-visual-selection)
                         (eq 'block evil-visual-selection)))
                (let ((cols (abs (- (dash-modeline-column end)
                                    (dash-modeline-column beg)))))
                  (format "%dx%dB" lines cols)))
               ((and (bound-and-true-p evil-visual-selection)
                     (eq evil-visual-selection 'line))
                (format "%dL" lines))
               ((> lines 1)
                (format "%dC %dL" (- end beg) lines))
               (t
                (format "%dC" (- end beg)))))
       'face (dash-modeline-face 'secondary)))))

;;;###autoload
(defun dash-modeline-prog-mode (&optional default)
  "Dash line for prog mode. Can be made DEFAULT mode."

  (funcall dash-modeline-position
            '((dash-modeline-buffer-status) " "
              (dash-modeline-buffer-name) " "
              ;; (dash-modeline-git-info) " "
              (dash-modeline-flymake) " "
              ;; (dash-modeline-flycheck) " "
              (dash-modeline-selection-info))
            '((dash-modeline-window-dedicated)
              (dash-modeline-cursor-position))
            default))

(defun dash-modeline-pdf-mode ()
  "Dash line for text mode"

  (funcall dash-modeline-position
           '((dash-modeline-buffer-status "PDF") " "
             (dash-modeline-buffer-name) " "
             (dash-modeline-file-size))
           '((dash-modeline-pdf-page)
             (dash-modeline-window-dedicated))))

(defun dash-modeline-term-mode ()
  "Dash line for term mode"

  (funcall dash-modeline-position
           '((dash-modeline-buffer-status ">_") " "
             (dash-modeline-term-shell-name))
           '((dash-modeline-window-dedicated)
             (dash-modeline-default-directory) " ")))

(defun dash-modeline-message-mode ()
  "Dash line for messages mode"

  (funcall dash-modeline-position
           '((dash-modeline-buffer-status "LOG") " "
             (dash-modeline-buffer-name) " "
             (dash-modeline-buffer-line-count))
           '((dash-modeline-window-dedicated))))

(defun dash-modeline-agenda-mode ()
  "Dash line for messages mode"

  (funcall dash-modeline-position
           '((dash-modeline-buffer-status "Agenda"))
           '((dash-modeline-datetime) " ")))

(provide 'dash-modeline)
;;; dash-modeline.el ends here
