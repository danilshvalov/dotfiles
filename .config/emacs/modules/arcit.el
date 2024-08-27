;;; arcit.el --- An Arc porcelain inside Emacs -*- lexical-binding:t; coding: utf-8 -*-
;;
;; Copyright (C) Яндекс
;;
;; Author: tazjin@
;; Package-Requires: ((emacs "28.1") cl-lib transient magit-section with-editor)
;;
;; Report bugs to https://nda.ya.ru/t/Duqv7dh374CG54 (or via `M-x
;; arcit-report-bug' (or manually in st/EMACS)).

;; Built-in dependencies
(require 'array)
(require 'button)
(require 'cl-lib)
(require 'diff-mode)
(require 'eieio)
(require 'json)
(require 'map)
(require 'parse-time)
(require 'rx)
(require 'seq)

;; Third-party dependencies
(require 'magit-section)
(require 'transient) ;; built-in since Emacs 29
(require 'with-editor)

(defgroup arcit nil "Configuration for arcit"
  :group 'tools)

(defgroup arcit-faces nil
  "Faces used by arcit."
  :group 'arcit
  :group 'faces)

(defcustom arcit-arc-executable "arc" "Executable for `arc'"
  :type 'string
  :group 'arcit)

(defcustom arcit-current-mount-path nil "Arcadia mount path"
  :type 'string
  :group 'arcit)

(defcustom arcit-arc-timeout 5 "How many seconds to wait before timing out `arc' processes"
  :type 'integer
  :group 'arcit)

;; Interactive sections

(defclass arcit-section (magit-section) nil
  :documentation "Magit-style sections for arcit, which provide additional
functionality.")

(defmacro define-section-handler (verb &optional docs)
  "Define a default section handler, that is an interactive command
which behaves differently based on the section at point. VERB is
a symbol which is used to construct the name of the generic
method, and the error message shown to the user if the section
does not support the action."
  (declare (indent defun))
  (let ((name (intern (format "arcit-%s-section-thing" verb))))
    `(cl-defmethod ,name ((obj arcit-section))
       ,docs
       (if-let ((parent (oref obj parent))
                (_ (obj-of-class-p parent arcit-section)))
           (,name parent)
         (message ,(format "Nothing to %s at point!" verb))))))

(define-section-handler visit
  "Visit the thing belonging to the given section. Overridden by
each section class that has a visitable thing.")

(define-section-handler copy
  "Copy the thing belonging to the given section. Overridden by each
section class that has a copyable thing.")

(define-section-handler delete
  "Delete the deletable thing belonging to the section. Overridden
by each section class that has a deletable thing.")

;; arc process handling
;;
;; arcit needs to asynchronously, and sometimes synchronously, launch
;; arc processes, and do different things with their output. The best
;; way to do this in Emacs is through the low-level `make-process'
;; helper, which allows for full flexibility in output handling and
;; synchronicity behaviour.

(define-derived-mode arcit-process-mode arcit-mode "Arcit-Process"
  "Mode for the buffer in which `arc' process invocations are logged.")

(defun arcit--process-log-buffer ()
  "Returns the buffer in which `arc' process invocations are logged."
  (let ((buffer (get-buffer-create "arcit-process-log"))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (unless (derived-mode-p 'arcit-process-mode)
        (arcit-process-mode)
        (magit-insert-section (arcit-root-section)
          (insert "\n"))))
    buffer))

(defun arcit--truncate-process-log ()
  (with-current-buffer (arcit--process-log-buffer)
    (when (derived-mode-p 'arcit-process-mode)
      (let* ((children (oref magit-root-section children))
             (head (car children))
             (tail (cdr children))
             (count (length children))
             (inhibit-read-only t))
        (while (and head (> count 32))
          (delete-region (oref head start)
                         (oref head end))
          (setq head (pop tail))
          (cl-decf count))
        (oset magit-root-section children tail)))))

(defclass arcit-process nil
  ((process :type process)
   (args :type list)
   (logged :type boolean :initform nil)
   (cleanup :documentation "Finalizer that cleans up associated resources.")

   (stdout-buffer :type buffer
                  :initform (generate-new-buffer " *arc-stdout*" t))
   (stderr-buffer :type buffer
                  :initform (generate-new-buffer " *arc-stderr*" t))

   ;; Output data
   (exit-code :type number
              :documentation "arc exit code")
   (stdout :documentation "arc standard output" :initform nil)
   (stderr :documentation "arc standard error" :initform nil))

  :documentation "Asynchronous `arc' invocation.")

(cl-defmethod initialize-instance :after ((obj arcit-process) &rest _)
  "Sets up the finaliser for an `arcit-process' instance."
  (with-slots (stdout-buffer stderr-buffer) obj
    (oset obj cleanup
          (make-finalizer (lambda ()
                            (arcit--log-process obj)
                            (when (buffer-live-p stdout-buffer)
                              (kill-buffer stdout-buffer))
                            (when (buffer-live-p stderr-buffer)
                              (kill-buffer stderr-buffer)))))))

(cl-defmethod arcit--stderr-handler ((obj arcit-process))
  "Internal helper for saving the stderr of an arc invocation."
  (make-pipe-process
   :name "arc-stderr-handler"
   :buffer (oref obj stderr-buffer)
   :noquery t
   :sentinel (lambda (proc _status)
               (unless (process-live-p proc)
                 (with-current-buffer (process-buffer proc)
                   (oset obj stderr (string-trim (buffer-string))))))))

(cl-defmethod arcit--make-arc-sentinel ((obj arcit-process) parse callback)
  (lambda (proc _status)
    (unless (process-live-p proc)
      (with-current-buffer (process-buffer proc)
        (oset obj exit-code (process-exit-status proc))
        (when (= 0 (process-exit-status proc))
          (oset obj stdout
                (pcase parse
                  ('json (goto-char (point-min))
                         (json-parse-buffer :object-type 'alist))
                  ('json-stream
                   (goto-char (point-min))
                   (let ((json-object-list))
                     (while (not (eobp))
                       (condition-case nil
                           (push (json-parse-buffer :object-type 'alist) json-object-list)
                         (end-of-file (goto-char (point-max)))))
                     (nreverse json-object-list)))
                  (_ (string-trim (buffer-string)))))))
      (when callback
        (funcall callback obj)))))

(defclass arcit-process-log-section (arcit-section) nil)

(cl-defmethod arcit--log-process ((obj arcit-process))
  (with-slots (args logged exit-code stdout stderr) obj
    (unless logged
      (let ((buffer (arcit--process-log-buffer)))
        (with-current-buffer buffer
          (goto-char (1- (point-max)))
          (let ((magit-insert-section--parent magit-root-section)
                (magit-insert-section--oldroot nil)
                (inhibit-read-only t)
                (inhibit-redisplay t))
            (magit-insert-section (arcit-process-log-section nil t)
              (magit-insert-heading (string-join (cons "$ arc" args) " "))
              (insert "exit code: " (number-to-string exit-code) "\n")
              (when (and stdout (not (seq-empty-p stdout)))
                (insert (format "stdout: %s\n" stdout)))
              (when (and stderr (not (seq-empty-p stderr)))
                (insert "stderr: " stderr "\n"))))
          (goto-char (point-max)))
        (arcit--truncate-process-log))
      (oset obj logged t))))

(defun arcit-async-arc (args &optional parse callback)
  "Asynchronously run an `arc' subprocess and optionally parses the
output if parse is set to `json' or `json-stream'. Returns the
process object.

Optional argument CALLBACK is called when the process exits."
  (let (;; set default directory, unless command doesn't require it
        (default-directory (pcase (car args)
                             ("mount" default-directory)
                             (_ (arcit-mount-path))))
        (proc (make-instance 'arcit-process)))
    (oset proc args args)
    (oset proc process
          (make-process :name "arc"
                        :command (cons arcit-arc-executable args)
                        :connection-type 'pipe
                        :noquery t
                        :buffer (oref proc stdout-buffer)
                        :stderr (arcit--stderr-handler proc)
                        :sentinel (arcit--make-arc-sentinel proc parse callback)))
    proc))

(cl-defmethod arcit-ensure-arc ((obj arcit-process))
  "Raises an error if the given arc process exited non-zero."
  (unless (= 0 (oref obj exit-code))
    (error "arc failed! %s" (if (slot-boundp obj 'stderr)
                                (oref obj stderr)
                              "empty stderr"))))

(cl-defmethod arcit-arc-running ((obj arcit-process))
  "Returns `t' if the process or its sentinel(s) are still running."
  (or (process-live-p (oref obj process))
      ;; check exit-code to see if sentinel has run
      (not (slot-boundp obj 'exit-code))))

(cl-defmethod arcit-await-arc ((obj arcit-process) &optional noraise)
  "Wait for the given asynchronous arc process to complete. Ignore
arc errors if NORAISE is set."
  (let ((start (current-time)))
    (while (arcit-arc-running obj)
      (when (> (time-to-seconds (time-since start)) arcit-arc-timeout)
        (error "Timed out waiting for: arc %s"
               (string-join (oref obj args) " ")))
      (accept-process-output (oref obj process) 1))

    (arcit--log-process obj)
    (unless noraise (arcit-ensure-arc obj))))

(defun arcit-sync-arc (args &optional parse noraise)
  "Synchronously run an `arc' subprocess, returning the populated
process object. Optional argument PARSE behaves as for
`arcit-async-arc'. Ignores arc errors if NORAISE is set."
  (let ((process (arcit-async-arc args parse)))
    (arcit-await-arc process noraise)
    process))

(defvar-local arcit--refresher nil
  "Internal buffer-local variable used to keep track of refreshing logic.")

(defun arcit-refresh ()
  "Refresh the current buffer. Exact behaviour depends on the mode."
  (interactive)
  (if (functionp arcit--refresher)
      (let ((line (current-line)))
        (message "Refreshing arcit buffer ...")
        (funcall arcit--refresher)
        (goto-char (point-min))
        (forward-line line))
    (error "Don't know how to refresh this arcit buffer!")))

(defun arcit-process-log ()
  "Show the `arc' process log buffer."
  (interactive)
  (switch-to-buffer (arcit--process-log-buffer)))

(defvar-keymap arcit-mode-map
  :doc "Keymap for all derived arcit modes."
  ;; Generic commands
  "RET" 'arcit-visit-thing
  "k" 'arcit-delete-thing
  "q" 'kill-current-buffer
  "C-w" 'arcit-copy-thing
  "g" 'arcit-refresh
  "]" 'arcit-report-bug
  "$" 'arcit-process-log

  ;; Dispatch to specific transients
  "c" 'arcit-commit
  "b" 'arcit-branch-dispatch
  "l" 'arcit-log
  "a" 'arcit-arcanum-dispatch
  "F" 'arcit-pull
  "P" 'arcit-push
  "r" 'arcit-rebase
  "z" 'arcit-stash
  "S" 'arcit-submit

  ;; The top-level transient is bound everywhere. It displays some
  ;; buffer local commands in specific modes, but unfortunately this
  ;; logic has to be controlled inside of the transient definition.
  "?" 'arcit-dispatch)

(define-derived-mode arcit-mode magit-section-mode "Arcit"
  "Major mode from which other Arcit modes inherit."
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq show-trailing-whitespace nil)
  (setq-local arcit-initialised t))

;; Bug reporting

(defun arcit-report-bug (&optional _ignored)
  "Open the page for reporting arcit bugs."
  (interactive)
  (let ((base "https://st.yandex-team.ru/createTicket?queue=EMACS&_form=184644"))
    (browse-url (if (derived-mode-p 'arcit-mode)
                    (concat base "&" (url-build-query-string
                                      `(("arcit-mode" ,(symbol-name major-mode)))))
                  base))))

(defclass arcit-bug-report-section (arcit-section) nil)

(defun arcit--insert-bug-report-section ()
  (magit-insert-section (arcit-bug-report-section)
    (insert "\n")
    (insert-text-button (propertize "🪲 Нашёл баг в arcit?" 'face 'arcit-dimmed)
                        'action #'arcit-report-bug)
    (insert "\n")))

;; Log display

(defvar-keymap arcit-log-mode-map
  :parent arcit-mode-map)

(define-derived-mode arcit-log-mode arcit-mode "Arcit-Log"
  "Major mode for `arcit-log' buffers.")

(defclass arcit-commit-section (arcit-section)
  ((commit :type string
           :documentation "Hash of the commit displayed in the section")))

(cl-defmethod arcit-visit-section-thing ((obj arcit-commit-section))
  (arcit-show-revision (oref obj commit)))

(cl-defmethod arcit-copy-section-thing ((obj arcit-commit-section))
  (kill-new (oref obj commit))
  (message "Copied commit hash: %s" (oref obj commit)))

(defun russian-relative-time (timestamp)
  "Return a human-friendly string representing the time elapsed since TIMESTAMP."
  (let* ((current (current-time))
         (diff (time-subtract current timestamp))
         (diff-in-seconds (float-time diff))
         (diff-in-minutes (/ diff-in-seconds 60))
         (diff-in-hours (/ diff-in-minutes 60))
         (diff-in-days (/ diff-in-hours 24))
         (diff-in-weeks (/ diff-in-days 7)))
    (cond
     ((< diff-in-seconds 60) "только что")
     ((< diff-in-seconds 300) "несколько минут назад")
     ((< diff-in-minutes 60) (format "%d минут назад" (round diff-in-minutes)))
     ((< diff-in-minutes 120) (format "1 час назад"))
     ((< diff-in-hours 24) (format "%d часов назад" (round diff-in-hours)))
     ((< diff-in-hours 48) "1 день назад")
     ((< diff-in-days 7) (format "%d дней назад" (round diff-in-days)))
     ((< diff-in-weeks 4) (format "%d недели назад" (round diff-in-weeks)))
     (t (format-time-string "%Y-%m-%d" timestamp)))))

(defface arcit-log-author
  '((((class color) (background light))
     :foreground "firebrick"
     :slant normal
     :weight normal)
    (((class color) (background  dark))
     :foreground "tomato"
     :slant normal
     :weight normal))
  "Face for the author part of the log output."
  :group 'arcit-faces)

(defvar-local arcit--required-margin-width 0
  "Width of the longest element displayed in the right margin.")

(defun arcit--margin-overlay (text &optional holder)
  "Inserts an overlay that displays TEXT in the margin at point. The
overlay apparently needs to be displayed, for this reason it is
inserted as a single space, which can be overriden by setting the
optional HOLDER."
  (setq arcit--required-margin-width (max arcit--required-margin-width
                                          (length text)))
  (let ((begin (point)))
    (insert (or holder " "))
    (let ((overlay (make-overlay begin (point))))
      (overlay-put overlay 'before-string (or holder " "))
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'display `((margin right-margin) ,text)))))

(defvar-local arcit--author-padding nil
  "Padding for longest author name, used for display purposes.")

(defun arcit--log-insert-commit (commit)
  (let* ((msg (car (string-lines (map-elt commit 'message))))
         (author (propertize (string-pad (map-elt commit 'author) arcit--author-padding)
                             'face 'arcit-log-author))
         (time (propertize (russian-relative-time
                            (parse-iso8601-time-string (map-elt commit 'date)))
                           'face 'arcit-dimmed))
         (margin-string (concat author " " time)))
    (magit-insert-section (arcit-commit-section)
      (oset magit-insert-section--current commit (map-elt commit 'commit))
      (insert (propertize (substring (map-elt commit 'commit) 0 8)
                          'face 'arcit-dimmed))
      (arcit--margin-overlay margin-string)
      (insert msg "\n"))))

(defun arcit-read-path (prompt &rest _unused)
  "Interactively prompt for a file path in arc. Setting MUST-EXIST
prevents entering file names which don't currently exist in
arc (e.g. deleted files)."
  (interactive)
  ;; Directory should end in slash, so that we don't end up completing
  ;; the arc mount path itself.
  (let* ((arc-dir (if (string-suffix-p "/" (arcit-mount-path))
                      (arcit-mount-path)
                    (format "%s/" (arcit-mount-path)))))
    (read-file-name prompt arc-dir nil nil nil)))

(defun arcit--set-log-window-margin ()
  (set-window-margins nil nil arcit--required-margin-width))

(defun arcit--display-log (revision &optional args)
  "Show a log buffer for REVISION using arguments read by
`arcit-log', or explicit ARGS if set.."
  (let* (;; Prepare `arc log' arguments
         (args (or args (transient-args 'arcit-log)))
         (split-args (seq-group-by (apply-partially #'string-prefix-p "--") args))
         (plain-args (map-elt split-args t))
         (file-args (seq-map #'string-trim (map-elt split-args nil)))
         (arc-log-args (flatten-list `("log" "--json" "-n50" ,plain-args ,revision ,file-args)))

         ;; Invoke `arc log' process.
         (proc (arcit-async-arc arc-log-args 'json))

         (buffer (get-buffer-create (concat "arc-log: " revision)))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (arcit-log-mode)
      (setq arcit--refresher (lambda () (arcit--display-log revision args)))
      (setq header-line-format (propertize (concat "Arc commits in " revision ":")
                                           'face 'magit-section-heading))
      (magit-insert-section (arcit-root-section)
        (arcit-await-arc proc)
        (let ((commits (oref proc stdout)))
          (setq arcit--author-padding
                (1+ (cl-loop for commit across commits
                             maximize (length (map-elt commit 'author)))))
          (seq-each #'arcit--log-insert-commit commits))
        (arcit--insert-bug-report-section))

      (setq-local window-configuration-change-hook
                  (cons #'arcit--set-log-window-margin window-configuration-change-hook))
      (goto-char (point-min)))
    (switch-to-buffer-other-window buffer)))

(transient-define-argument arcit:-- ()
  :description "Limit to path"
  :class 'transient-files
  :key "--"
  :argument " "
  :prompt "Limit to path: "
  :reader #'arcit-read-path)

(transient-define-argument arcit-log:--after ()
  :description "Commits after date"
  :class 'transient-option
  :key "-A"
  :argument "--after="
  :reader #'transient-read-date)

(transient-define-argument arcit-log:--before ()
  :description "Commits before date"
  :class 'transient-option
  :key "-B"
  :argument "--before="
  :reader #'transient-read-date)

(defun arcit-log-current ()
  "Show log of the arc rev/ref of the section at point, or show the
current HEAD."
  (interactive)
  (let ((section (magit-current-section)))
    (arcit--display-log (if (arcit-commit-section-p section)
                            (oref section commit)
                          "HEAD"))))

(transient-define-prefix arcit-log ()
  "Display an arc log based on the given options."
  [["Commit filtering"
    ("-a" "Limit to author" "--author=" :prompt "Staff login: ")
    ("-g" "Search in commit message" "--grep=" :prompt "Commit message search string: ")
    ("-S" "Search in changes" "--search=" :prompt "Change search string: ")
    (arcit-log:--before)
    (arcit-log:--after)]
   ["File filtering"
    (arcit:--)
    ("-f" "Follow file renames" "--follow")]]
  ["Log"
   ("l" "Log current" arcit-log-current)
   ("h" "Log HEAD" (lambda () (interactive) (arcit--display-log "HEAD")))
   ("b" "Log branch" (lambda () (interactive) (arcit--display-log (arcit-read-branch "Log branch: "))))
   ("o" "Log other" (lambda () (interactive)
                      (arcit--display-log (arcit-read-branch "Show log of: "))))
   ("t" "Log trunk" (lambda () (interactive) (arcit--display-log "trunk")))])

(defun arcit-log-buffer-file (follow)
  "Show the log for the arc file in the current buffer. It will
follow renames if the prefix argument is set."
  (interactive "P")
  (if-let ((file (buffer-file-name)))
      (if (string-prefix-p (arcit-mount-path) file)
          (arcit--display-log "HEAD" (flatten-list (list (when follow "--follow")
                                                         file)))
        (user-error "This file is not in the default arc path! (%s)" (arcit-mount-path)))
    (user-error "This buffer is not visiting a file!")))

;; Fetching, pulling & pushing

(defun arcit--run-pull (branch)
  (let ((args (transient-args 'arcit-pull)))
    (message "Running arc pull ...")
    (arcit-sync-arc (flatten-list (list "pull" args branch)))
    (if branch (message "Pulled branch %s" branch)
      (message "Pulled current branch"))
    (when (derived-mode-p 'arcit-status-mode)
      (arcit-refresh))))

(transient-define-prefix arcit-pull ()
  "Invoke `arc pull' interactively."
  ["Options:"
   ("-r" "Rebase if branches diverge" "--rebase")
   ("-f" "Force checkout (overwrite local change)" "--force")
   ("-F" "Fast-forward only" "--ff-only")]
  ["Pull branch:"
   ("f" "Current branch" (lambda () (interactive) (arcit--run-pull nil)))
   ("t" "trunk" (lambda () (interactive) (arcit--run-pull "trunk")))
   ("o" "Other branch" (lambda () (interactive)
                         (arcit--run-pull (arcit-read-branch "Pull branch: "))))])

(defun arcit--run-push ()
  (arcit-sync-arc (flatten-list (list "push" (transient-args 'arcit-push))))
  (message "Pushed current branch")
  (when (derived-mode-p 'arcit-status-mode)
    (arcit-refresh)))

(transient-define-prefix arcit-push ()
  "Invoke `arc push` interactively."
  ["Options:"
   ("-d" "Delete branch on server" "--delete")
   ("-f" "Force-push updates" "--force")
   ("-p" "Publish latest diff-set in PR" "--publish")
   ("-nv" "Disable the pre-push hook" "--no-verify")]
  [("p" "Run arc push" (lambda () (interactive) (arcit--run-push)))])

;; Rebasing

(defun arcit--run-rebase (&optional extra-args)
  "Invoke `arc rebase', always asynchronously because any invocation
could be interactive."
  (with-editor
    (arcit-async-arc (flatten-tree `("rebase" ,(transient-args 'arcit-rebase) ,extra-args))
                     nil (lambda (p)
                           ;; give stderr sentinel time to finish ...
                           (arcit-await-arc p)
                           (arcit--refresh-status (arcit--get-status-buffer))))))

(defun arcit-rebase-branch (branch)
  "Rebase the current HEAD onto the target BRANCH. Prompts users
interactively for a branch when called, but also allows pasting
revisions and commit IDs."
  (interactive (list (arcit-read-branch "Rebase onto: ")))
  (arcit--run-rebase (list branch)))

(defun arcit-rebase-trunk (prefix)
  "Rebase the current HEAD onto the target trunk. If PREFIX is set,
pulls trunk first."
  (interactive "P")
  (when prefix
    (arcit--run-pull "trunk"))
  (arcit--run-rebase '("trunk")))

(transient-define-prefix arcit-rebase ()
  "Invoke `arc rebase' interactively."
  :incompatible '(("--virtual" "--checkout-conflicts"))

  ["Options:"
   ("-i" "interactively edit list of commits to rebase" "--interactive")
   ("-a" "automatically modify todo list to reorder squashes/fixups" "--autosquash")
   ("-e" "keep commits without changes after rebase" "--empty=" :choices ("drop" "keep"))
   ("-C" "checkout if conflict occured, do not checkout on success" "--checkout-conflicts")
   ("-v" "no checkouts, but abort if conflict occured" "--virtual")
   ("-X" "explicitly set merge strategy" "-X=")
   ("-P" "consider all parents instead of only the first one" "--all-parents")
   ("-M" "include merge commits into rebase (dangerous!)" "--include-merges")]
  [["Rebase:"
    ("t" "current branch onto trunk" arcit-rebase-trunk)
    ("b" "current branch onto other branch" arcit-rebase-branch)]])

;; Stashing

(defclass arcit-stash-section (arcit-section) nil)

(defclass arcit-stash-entry-section (arcit-section)
  ((stash-id :type string)
   (stash-seq :type integer)))

(cl-defmethod arcit-visit-section-thing ((obj arcit-stash-entry-section))
  (arcit-show-revision (oref obj stash-id)))

(cl-defmethod arcit-copy-section-thing ((obj arcit-stash-entry-section))
  (kill-new (oref obj stash-id))
  (message "Copied hash of stash entry"))

(cl-defmethod arcit-delete-section-thing ((obj arcit-stash-entry-section))
  (when (yes-or-no-p (format "Drop stash@{%s}?" (oref obj stash-seq)))
    (arcit-sync-arc `("stash" "drop" ,(oref obj stash-id)))
    (arcit-refresh)))

(defun arcit--stash-at-point-p ()
  (arcit-stash-entry-section-p (magit-current-section)))

(defun arcit-stash-both ()
  "Stash all changes in tracked files, including change that have
already been staged. If `--include-untracked' is set, will also
stash untracked files in the repository."
  (interactive)
  (arcit-sync-arc (append '("stash" "push") (transient-args 'arcit-stash)))
  (arcit-refresh))

(defun arcit-stash-worktree ()
  "Stash all changes in tracked files, except those that have
already been staged."
  (interactive)
  (arcit-sync-arc (append '("stash" "push" "--keep-index") (transient-args 'arcit-stash)))
  (arcit-refresh))

(defun arcit-stash-apply ()
  "Apply the current top of the stash, or the stash entry at point."
  (interactive)
  (arcit-sync-arc (flatten-list `("stash" "apply" ,(when (arcit--stash-at-point-p)
                                                     (oref (magit-current-section) stash-id)))))
  (arcit-refresh))

(defun arcit-stash-pop ()
  "Pop the current top of the stash, or the stash entry at point."
  (interactive)
  (arcit-sync-arc (flatten-list `("stash" "pop" ,(when (arcit--stash-at-point-p)
                                                   (oref (magit-current-section) stash-id)))))
  (arcit-refresh))

(defun arcit-stash-drop ()
  "Drop the current top of the stash, or the stash entry at point."
  (interactive)
  (arcit-sync-arc (flatten-list `("stash" "drop" ,(when (arcit--stash-at-point-p)
                                                    (oref (magit-current-section) stash-id)))))
  (arcit-refresh))

(transient-define-prefix arcit-stash ()
  "Invoke `arc stash' interactively."
  ["Options:"
   ("-u" "stash untracked files" "--include-untracked")
   ("-m" "stash message" "--message=")]
  [["Stash"
    ("z" "all" arcit-stash-both)
    ("w" "worktree" arcit-stash-worktree)]
   [:description (lambda () (if (arcit--stash-at-point-p) "Use stash at point"
                              "Use latest stash"))
                 ("a" "Apply" arcit-stash-apply)
                 ("p" "Pop" arcit-stash-pop)
                 ("k" "Drop"  arcit-stash-drop)]])

(defun arcit--insert-stash-section (proc)
  (arcit-await-arc proc)
  (unless (seq-empty-p (oref proc stdout))
    (let ((seq-id 0))
      (magit-insert-section (arcit-stash-section)
        (magit-insert-heading "Stashes")
        (seq-do

         (lambda (stash)
           (magit-insert-section (arcit-stash-entry-section (map-elt stash 'id))
             (oset magit-insert-section--current stash-id (map-elt stash 'id))
             (oset magit-insert-section--current stash-seq seq-id)
             (insert (propertize (format "stash@{%d}" seq-id)  'face 'arcit-dimmed)
                     " " (map-elt stash 'description) "\n"))
           (cl-incf seq-id))
         (oref proc stdout))
        (insert "\n")))))

;; arc submit

(defun arcit-call-submit ()
  (interactive)
  (with-editor
    (arcit-async-arc (append '("submit") (transient-args 'arcit-submit))
                     nil (lambda (_) (arcit--refresh-status (arcit--get-status-buffer))))))

(transient-define-prefix arcit-submit ()
  "Invoke `arc submit' interactively."
  ["Options:"
   ("-A" "automatically publish, automerge and remove review requirement" "--auto")
   ("-M" "enable automatic merging" "--merge")
   ("-ne" "use the last commit message without launching an editor" "--no-edit")
   ("-nr" "disable code review check" "--no-code-review")]
  ["Submit:"
   ("s" "submit changes for review" arcit-call-submit)])

;; Status display

(defvar-keymap arcit-status-mode-map
  :doc "Key map for the `arcit-status-mode'"
  :parent arcit-mode-map
  "s" 'arcit-stage-file
  "u" 'arcit-unstage-file)

(define-derived-mode arcit-status-mode arcit-mode "Arcit"
  "Major mode for the Arcit status buffer."
  :group 'arcit
  (setq-local default-directory (arcit-mount-path))
  (hl-line-mode 1))

(defun arcit--get-status-buffer ()
  (let ((buffer (get-buffer-create (concat "arcit-status: " (arcit-mount-path)))))
    (with-current-buffer buffer
      (if (bound-and-true-p arcit-initialised) buffer
        (arcit-status-mode)
        buffer))))

(defun arcit--handle-status-output (process)
  (let*  ((parsed (oref process stdout))
          (local (map-nested-elt parsed '(branch_info local))))
    (list (cons :branch (map-elt local 'name))
          (cons :revision (map-nested-elt local '(commit id)))
          (cons :message (map-nested-elt local '(commit title)))
          (cons :unmerged (map-nested-elt parsed '(status unmerged)))
          (cons :untracked (map-nested-elt parsed '(status untracked)))
          (cons :staged (map-nested-elt parsed '(status staged)))
          (cons :changed (map-nested-elt parsed '(status changed))))))

(defface arcit-branch-local
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for local branches."
  :group 'arcit-faces)

(defface arcit-branch-remote
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background  dark)) :foreground "DarkSeaGreen2"))
  "Face for remote branch head labels shown in logs."
  :group 'arcit-faces)

(defface arcit-branch-current
  '((((supports (:box t))) :inherit arcit-branch-local :box t)
    (t                     :inherit arcit-branch-local :inverse-video t))
  "Face for current branch."
  :group 'arcit-faces)

(defface arcit-dimmed
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey50"))
  "Face for text that shouldn't stand out."
  :group 'arcit-faces)

(defun arcit--file-at-point-p ()
  (arcit-file-section-p (magit-current-section)))

(transient-define-prefix arcit-dispatch ()
  "Invoke an Arcit command from a list of available commands."
  ["Arc commands"
   ("c" "       Commit" arcit-commit :transient t)
   ("b" "       Branch/checkout" arcit-branch-dispatch :transient t)
   ("a" "       Arcanum" arcit-arcanum-dispatch :transient t)
   ("l" "       Log" arcit-log :transient t)
   ("F" "       Pull" arcit-pull :transient t)
   ("P" "       Push" arcit-push)
   ("r" "       Rebase" arcit-rebase :transient t)
   ("z" "       Stash" arcit-stash)
   ("S" "       Submit" arcit-submit :transient t)]

  ["Applying changes"
   :if-mode arcit-status-mode
   :if arcit--file-at-point-p
   ("s" "       Stage file" arcit-stage-file)
   ("u" "       Unstage file" arcit-unstage-file)]

  ["Essential commands"
   :if-derived arcit-mode
   [("g" "       Refresh current buffer" arcit-refresh)
    ("k" "       Delete thing at point" arcit-delete-thing)
    ("RET" "     Visit thing at point" arcit-visit-thing)
    ("C-w" "     Copy thing at point" arcit-copy-thing)
    ("$" "       Show arc process logs" arcit-process-log)
    ("]" "       Report a bug (🪲)" arcit-report-bug)]])

(defun arcit-copy-thing ()
  "Copies whatever is currently copyable at point, if anything!"
  (interactive)
  (arcit-copy-section-thing (magit-current-section)))

(defun arcit-delete-thing ()
  "Deletes whatever is deletable at point, if anything."
  (interactive)
  (arcit-delete-section-thing (magit-current-section)))

(defun arcit-visit-thing ()
  "Visit the thing belonging to the section at point, if it is
visitable. Visiting logic is entirely within the section classes."
  (interactive)

  (arcit-visit-section-thing (magit-current-section)))

(defclass arcit-file-list-section (arcit-section) nil)
(defclass arcit-file-hunk-section (arcit-section) nil)

(defclass arcit-file-section (arcit-section)
  ((path :type string
         :documentation "Path of the file being displayed.")
   (state :type symbol
          :documentation "Arc state that the file is in.")))

(defun arcit-stage-file ()
  (interactive)

  (let ((section (magit-current-section)))
    (unless (arcit-file-section-p section)
      (user-error "Point is not on a file, cannot stage this!"))

    (when (equal 'staged (oref section state))
      (user-error "File is already staged!"))

    (arcit-sync-arc `("add" ,(oref section path)))
    (arcit-refresh)))

(defun arcit-unstage-file ()
  (interactive)

  (let ((section (magit-current-section)))
    (unless (arcit-file-section-p section)
      (user-error "Point is not on a file, cannot stage this!"))

    (unless (equal 'staged (oref section state))
      (user-error "File is not staged!"))

    (arcit-sync-arc `("reset" ,(oref section path)))
    (arcit-refresh)))

(cl-defmethod arcit-visit-section-thing ((obj arcit-file-section))
  "Open the section's file."
  (find-file (oref obj path)))

(cl-defmethod arcit-copy-section-thing ((obj arcit-file-section))
  (kill-new (oref obj path))
  (message "Copied file path: %s" (oref obj path)))

(cl-defmethod arcit-delete-section-thing ((obj arcit-file-section))
  (let ((path (oref obj path)))
    (pcase (oref obj state)
      ('untracked (when (yes-or-no-p (format "Do you want to delete the file '%s'?" path))
                    (delete-file path nil)
                    (message "Deleted file '%s'" path)))
      ('unstaged (when (yes-or-no-p (format "Discard unstaged changes in file '%s'?" path))
                   (arcit-sync-arc `("checkout" "HEAD" ,path))
                   (message "Discarded changes in '%s'" path)))
      ('staged (when (yes-or-no-p (format "Discard staged changes in file '%s'?" path))
                 (arcit-sync-arc `("checkout" "HEAD" ,path))
                 (message "Discarded changes in '%s'" path)))
      (_ (message "Don't know how to delete/undo this file!")))
    (arcit-refresh)))

(defun arcit--split-diff (diff)
  "Splits DIFF into hunks."
  (with-temp-buffer
    (insert diff)
    (goto-char 0)
    (re-search-forward "^@@")
    (let ((result)
          (begin (line-beginning-position)))
      (while (re-search-forward "^@@" nil t)
        (setq result (append result (list (buffer-substring begin (line-beginning-position)))))
        (setq begin (line-beginning-position)))
      (setq result (append result (list (buffer-substring begin (buffer-end 1))))))))

(defun arcit--status-file-section (heading file-state files
                                           &optional diff-proc diff-cache
                                           bold show-status)
  "Insert a section with FILES in FILE-STATE in the current status
buffer, if there are any such files. For staged/unstaged files,
diffs are displayed in the file's section (collapsed by default)
if DIFF-PROC and DIFF-CACHE are set.

Highlights file names if BOLD is set. Shows the detailed status
if SHOW-STATUS is set."
  (when (not (seq-empty-p files))
    (magit-insert-section (arcit-file-list-section file-state)
      (magit-insert-heading heading)
      (seq-do
       (lambda (file)
         (magit-insert-section (arcit-file-section (map-elt file 'path) nil)
           (oset magit-insert-section--current path (map-elt file 'path))
           (oset magit-insert-section--current state file-state)

           (magit-insert-heading
             (propertize
              (concat (when show-status (string-pad (map-elt file 'status) 11))
                      (map-elt file 'path) "\n")
              'face (if bold 'bold 'default)))

           (unless (or (equal 'untracked file-state)
                       (equal 'unmerged file-state))
             (magit-insert-section-body
               (if-let* ((_ (and diff-cache diff-proc))
                         (diffs (arcit--parse-file-diffs-with-cache
                                 diff-cache diff-proc))
                         (file-diff (map-elt diffs (map-elt file 'path)))
                         (hunks (arcit--split-diff file-diff)))
                   (dolist (hunk hunks)
                     (magit-insert-section (arcit-file-hunk-section)
                       (let ((header-end (1+ (string-match "\n" hunk))))
                         (magit-insert-heading (substring hunk 0 header-end))
                         (insert (substring hunk header-end nil)))))
                 (insert "No diff available for this file.\n"))))))
       files)

      (insert "\n"))))

(defclass arcit-log-section (arcit-section) nil)

(defun arcit--shorten-remote-branch (branch)
  (with-temp-buffer
    (insert branch)
    (goto-char (point-min))
    (format-replace-strings '(("arcadia/users/" . "a/u/")))
    (buffer-string)))

(defun arcit--insert-log-section-body (log-output &optional async-parent)
  (if (seq-empty-p log-output)
      (insert "No new commits ahead of trunk!")

    (cl-loop for commit across log-output
             do
             (magit-insert-section (arcit-commit-section)
               (oset magit-insert-section--current commit (map-elt commit 'commit))
               ;; when asynchronously rerendering the log section,
               ;; we're not inside the macro that can update the
               ;; children, so we have to do it manually
               (when async-parent
                 (push magit-insert-section--current (oref async-parent children)))
               (insert
                (propertize
                 (string-join
                  (flatten-list
                   (list
                    (propertize (substring (map-elt commit 'commit) 0 10)
                                'face 'arcit-dimmed)

                    ;; display local branch names
                    (when-let ((local (map-nested-elt commit '(branches local))))
                      (seq-map
                       (lambda (branch)
                         (propertize branch 'face 'arcit-branch-local))
                       local))

                    ;; display remote branch names
                    (when-let ((remote (map-nested-elt commit '(branches remote))))
                      (seq-map (lambda (branch)
                                 (propertize (arcit--shorten-remote-branch branch)
                                             'face 'arcit-branch-remote))
                               remote))

                    (car (string-split (map-elt commit 'message) "\n"))
                    "\n"))
                  " ")))))))

(defun arcit--status-log-section (log-process)
  ;; arc log can take a long time to complete, so rendering the log
  ;; asynchronously is sometimes required.
  ;;
  ;; to deal with this, we insert a placeholder in the body and
  ;; refresh it once the process is done.
  (magit-insert-section (arcit-log-section)
    (let ((log-running (arcit-arc-running log-process))
          (buf (current-buffer))
          (log-section magit-insert-section--current))
      (magit-insert-heading "Commits ahead of trunk:")
      (if log-running
          (progn
            (insert (propertize  "Loading commits ...\n" 'face 'arcit-dimmed))
            ;; unlock log-process so that another thread can wait for it
            (set-process-thread (oref log-process process) nil)
            (make-thread
             (lambda ()
               (let ((arcit-arc-timeout (max arcit-arc-timeout 20)))
                 (arcit-await-arc log-process))
               (with-current-buffer buf
                 (let ((inhibit-read-only t))
                   (save-excursion
                     (with-slots (content end) log-section
                       (goto-char (marker-position content))
                       (delete-region (point) (marker-position end))
                       (arcit--insert-log-section-body (oref log-process stdout)
                                                       log-section)
                       (set-marker end (point))
                       (magit-insert-child-count log-section))))))
             "arcit-log-awaiter"))
        (arcit--insert-log-section-body (oref log-process stdout))))))

(defclass arcit-root-section (arcit-section) nil)
(defclass arcit-current-commit-section (arcit-commit-section) nil)

(defclass arcit-status-pr-section (arcit-section)
  ((pr-id :type string)))

(cl-defmethod arcit-visit-section-thing ((obj arcit-status-pr-section))
  (browse-url (concat "https://a.yandex-team.ru/review/" (oref obj pr-id))))

(cl-defmethod arcit-copy-section-thing ((obj arcit-status-pr-section))
  (kill-new (format "https://a.yandex-team.ru/review/%s/details" (oref obj pr-id)))
  (message "Copied URL of pull-request"))

(defun arcit--parse-file-diffs (proc)
  "Parses an `arc diff' output and returns an alist of the files
mapped to their respective hunks. The hunks will have the text
properties set by `diff-mode'."
  (arcit-await-arc proc)
  (let ((diffs))
    (with-temp-buffer
      (insert (oref proc stdout) "\n")
      (goto-char (point-min))
      (diff-mode)
      (font-lock-ensure)

      ;; Retain faces from overlays
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when-let ((face (overlay-get ov 'face)))
          (add-text-properties (overlay-start ov) (overlay-end ov)
                               `(face ,face))))

      ;; Skip over initial non-diff data (e.g. `arc show' output).
      (unless (looking-at diff-file-header-re)
        (diff-file-next))

      (while (not (eobp))
        (push (cons (substring-no-properties (car (diff-hunk-file-names)))
                    (let ((file-diff (apply #'buffer-substring (diff-bounds-of-file))))
                      (with-temp-buffer
                        (insert file-diff)
                        (goto-char (point-min))
                        (diff-hunk-next)
                        (delete-region (point-min) (point))
                        (buffer-string))))
              diffs)
        (diff-file-next)))
    diffs))

(defun arcit--parse-file-diffs-with-cache (cache-var proc)
  (or (symbol-value cache-var)
      (set cache-var (arcit--parse-file-diffs proc))))

(defvar-local arcit--status-staged-diffs nil)
(defvar-local arcit--status-unstaged-diffs nil)

(defun arcit-commit-at-point ()
  "Returns the commit hash at point, if any."
  (when-let ((section (magit-current-section)))
    (when (arcit-commit-section-p section)
      (oref section commit))))

(defun arcit--refresh-status (buffer)
  (with-current-buffer buffer
    ;; Configure refreshes
    (setq arcit--refresher (lambda () (arcit--refresh-status buffer)))

    ;; Reset per-refresh vars
    (setq arcit--status-staged-diffs nil)
    (setq arcit--status-unstaged-diffs nil)

    (let* ((inhibit-read-only t)
           ;; Asynchronously begin fetching status ...
           (arc-status-process (arcit-async-arc '("status" "--json" "--branch"
                                                  ;; include untracked files
                                                  "-u" "all")
                                                'json))

           (arc-pr-process (arcit-async-arc '("pr" "status" "--json") 'json))
           (arc-stash-process (arcit-async-arc '("stash" "list" "--json") 'json))

           ;; Synchronously determine merge-base (needed to start log fetch).
           (merge-base (oref (arcit-sync-arc '("merge-base" "HEAD" "trunk"))
                             stdout))

           ;; Asynchronously begin fetching log ...
           (log-range (format "%s..%s" merge-base "HEAD"))
           (arc-log-process (arcit-async-arc `("log" "--json" ,log-range) 'json))

           ;; Asynchronously begin fetching staged & unstaged diffs ...
           ;; (these are only used if a diff section is expanded)
           (arc-staged-diff-proc (arcit-async-arc '("diff" "--staged")))
           (arc-unstaged-diff-proc (arcit-async-arc '("diff")))

           ;; Begin actually preparing status output ...
           (arc-status (progn (arcit-await-arc arc-status-process)
                              (arcit--handle-status-output arc-status-process)))
           (current-revision (map-elt arc-status :revision)))
      (erase-buffer)

      (magit-insert-section (arcit-root-section)
        ;; Current commit information
        (magit-insert-section (arcit-current-commit-section)
          (oset magit-insert-section--current commit current-revision)
          (insert (propertize
                   (format "Head:\t%s %s\n"
                           (or (when-let ((branch (map-elt arc-status :branch)))
                                 (propertize branch 'face 'arcit-branch-current))
                               (substring current-revision 0 7))
                           (map-elt arc-status :message "no commit message"))))

          (insert "\n"))

        ;; Insert PR information, if there is a PR. Currently arc
        ;; exits non-zero if there is no PR.
        (when (= 0 (progn (arcit-await-arc arc-pr-process t)
                          (oref arc-pr-process exit-code)))
          (when-let* ((pr (oref arc-pr-process stdout))
                      (pr-id (number-to-string (map-elt pr 'id))))
            (magit-insert-section (arcit-status-pr-section)
              (oset magit-insert-section--current pr-id pr-id)
              (magit-insert-heading (concat "Current pull request (" pr-id ")"))
              (insert "Summary: " (map-elt pr 'summary) "\n")
              (insert "Status:  " (map-elt pr 'status) "\n")
              (when (not (seq-empty-p (map-elt pr 'issues)))
                (insert "Tickets: ")
                (arcit--insert-ticket-links (map-elt pr 'issues))
                (insert "\n"))
              (insert "\n"))))

        (arcit--status-file-section "Untracked files:" 'untracked (map-elt arc-status :untracked))
        (arcit--status-file-section "Unmerged files:" 'unmerged (map-elt arc-status :unmerged))
        (arcit--status-file-section "Unstaged files:" 'unstaged (map-elt arc-status :changed)
                                    arc-unstaged-diff-proc
                                    'arcit--status-unstaged-diffs t t)
        (arcit--status-file-section "Staged files:" 'staged (map-elt arc-status :staged)
                                    arc-staged-diff-proc
                                    'arcit--status-staged-diffs t t)

        ;; Display current stashes
        (arcit--insert-stash-section arc-stash-process)

        ;; Display log of commits (to trunk ...)
        (arcit--status-log-section arc-log-process)

        (arcit--insert-bug-report-section)))))

(defun arcit--commit (&optional suffix-args)
  ;; Calls commit inside a with-editor form, which configures the
  ;; process filter to set required envvars for opening the commit
  ;; window in emacsclient.
  (with-editor
    (arcit-async-arc (cons "commit" (append suffix-args (transient-args 'arcit-commit)))
                     nil (lambda (_) (arcit--refresh-status (arcit--get-status-buffer))))))

(defun arcit-commit-create ()
  "Create an arc commit with the options set in `arcit-commit'."
  (interactive)
  (arcit--commit))

(defun arcit-commit-extend ()
  "Amend the most recent arc commit, without editing the commit
message."
  (interactive)
  (arcit--commit '("--amend" "--no-edit")))

(defun arcit-commit-amend ()
  "Amend the most recent arc commit."
  (interactive)
  (arcit--commit '("--amend")))

(defun arcit-commit-fixup ()
  "Create a fixup for the commit at point, with currently staged
changes."
  (interactive)
  (if-let ((commit (arcit-commit-at-point)))
      (arcit--commit `("--fixup" ,commit))
    (user-error "No commit at point!")))

(defun arcit-commit-squash ()
  "Create a squash commit for the commit at point, with currently
staged changes."
  (interactive)
  (if-let ((commit (arcit-commit-at-point)))
      (arcit--commit `("--squash" ,commit))
    (user-error "No commit at point!")))

(defun arcit-commit-instant-fixup ()
  "Immediately fix up the commit at point with currently staged
changes. Note that this uses `autosquash', so any `squash!' or
`fixup!' commits in between the referenced commits will also be
affected."
  (interactive)
  (if-let ((commit (arcit-commit-at-point)))
      (progn
        (message "Fixing up changes into commit %s ..." commit)
        (arcit-sync-arc `("commit" "--fixup" ,commit))
        (arcit-sync-arc `("rebase" "--autosquash" ,(concat commit "~1")))
        (message "Fixed up commit %s." commit)
        (arcit--refresh-status (arcit--get-status-buffer)))
    (user-error "No commit at point!")))

(transient-define-prefix arcit-commit ()
  "Interactively commit staged changes in the repository."
  ["Arguments:"
   ("-a" "commit all changed files" "--all")
   ("-n" "bypass pre-commit hooks" "--no-verify")
   ("-s" "skip specific pre-commit hook" "--skip-hook=")
   ("-f" "commit even if prohibited by config" "--force")
   ("-r" "discard original author when amending" "--reset-author")]
  [["Create:"
    ("c" "Commit" arcit-commit-create)]
   ["Edit HEAD:"
    ("e" "Extend commit" arcit-commit-extend)
    ("a" "Amend commit" arcit-commit-amend)]
   ["Edit:"
    ("f" "Fixup" arcit-commit-fixup)
    ("s" "Squash" arcit-commit-squash)
    ("F" "Instant fixup" arcit-commit-instant-fixup)]])

(defun arcit-nyi ()
  (interactive)
  (user-error "This feature is not yet implemented!"))

(defun arcit-read-branch (prompt &optional initial history)
  "Read a branch name from arc."
  (let* ((branch-output (oref (arcit-sync-arc '("branch" "--json") 'json) stdout))
         (branches (seq-map (lambda (branch)
                              (map-elt branch 'name))
                            branch-output)))
    (completing-read prompt branches nil nil initial history)))

(defun arcit-checkout-branch (&optional current branch)
  "Checks out BRANCH, or prompts the user to select a branch if not given."
  (interactive)
  (arcit-sync-arc
   `("checkout"
     ,(or branch (arcit-read-branch "Which branch to checkout? " current))))
  (arcit-refresh))

(defun arcit-checkout-trunk ()
  (interactive)
  (arcit-checkout-branch nil "trunk"))

(defun arcit-select-pull-request (&optional pr filter)
  "Interactively select a pull request, unless PR is already set.
The optional FILTER parameter can be set to `incoming' or
`outgoing' to limit the set of displayed pull-requests."
  (or pr (let* ((pr-list (oref (arcit-sync-arc (append '("pr" "list" "--json")
                                                       (pcase filter
                                                         ('incoming '("--in"))
                                                         ('outgoing '("--out"))))
                                               'json-stream)
                               stdout))
                (candidates (seq-map (lambda (pr)
                                       (concat (int-to-string (map-elt pr 'id))
                                               " | "
                                               (map-elt pr 'summary)))
                                     pr-list)))
           (car (string-split (completing-read "Pull request: " candidates )
                              (rx (not digit)))))))

(defun arcit-checkout-pull-request (&optional filter)
  "Interactively check out a pull request."
  (interactive)
  (message "Loading pull requests ...")
  (arcit-sync-arc (list "pr" "checkout" (arcit-select-pull-request nil filter)))
  (arcit-refresh))

(defun arcit-create-branch (name)
  (interactive "sName for new branch: ")
  ;; TODO: validate branch name
  (arcit-sync-arc `("checkout" "-b" ,(string-trim name)))
  (arcit-refresh))

(transient-define-prefix arcit-branch-dispatch ()
  "Display options for working with branches and checking out things."
  ["Checkout/branch:"
   ("b" "branch/revision" arcit-checkout-branch)
   ("t" "trunk" arcit-checkout-trunk)
   ("p" "pull request" arcit-checkout-pull-request)
   ("c" "new branch" arcit-create-branch)])

(defun arcit-call-pr-create ()
  (interactive)
  (with-editor
    (arcit-async-arc (append '("pr" "create" "--json") (transient-args 'arcit-create-pr))
                     nil (lambda (_) (arcit--refresh-status (arcit--get-status-buffer))))))

(transient-define-prefix arcit-create-pr ()
  "Interactively create a pull-request from the current branch."
  ["Create options:"
   ("-A" "automatically publish, automerge and remove review requirement" "--auto")
   ("-M" "enable automatic merging" "--merge")
   ("-ne" "do not edit commit messages for PR" "--no-edit")
   ("-nc" "do not include commit messages in PR's description" "--no-commits")
   ("-nr" "disable code review check" "--no-code-review")
   ("-nv" "do not run pre-push hooks" "--no-verify")]
  ["Pull Request:"
   ("c" "create pull request" arcit-call-pr-create)])

(transient-define-prefix arcit-arcanum-dispatch ()
  "Dispay commands related to working with Arcanum."
  ["Arcanum:"
   ("c" "Create PR" arcit-create-pr)
   ("o" "Checkout outgoing pull-request" (lambda () (interactive)
                                           (arcit-checkout-pull-request 'outgoing)))
   ("i" "Checkout incoming PR" (lambda () (interactive)
                                 (arcit-checkout-pull-request 'incoming)))])

(defun arcit-mount-path ()
  "Returns the first active Arc mount path, or the value of
arcit-mount-path."
  (or arcit-current-mount-path
      ;; TODO(tazjin): mount writes to stderr - arc bug?
      (if-let* ((mounts
                 (string-split (oref (arcit-sync-arc '("mount" "-l")) stderr)
                               "\n" t split-string-default-separators))
                (mount-line (seq-find (lambda (s) (string-match-p (regexp-quote "[mounted,") s))
                                      mounts))
                (mount (when (string-match (rx "mount: " (group (+? (not space))) ", ") mount-line)
                         (match-string 1 mount-line))))
          (progn
            (setq arcit-current-mount-path mount)
            mount)
        (error "arc reported no mounted copies of Arcadia!"))))

(defun arcit-status ()
  "Show the current status of Arc in a buffer."
  (interactive)
  (let ((default-directory (arcit-mount-path))
        (buffer (arcit--get-status-buffer)))
    (arcit--refresh-status buffer)
    (switch-to-buffer buffer)))

(defvar-keymap arcit-revision-mode-map
  :parent arcit-mode-map)

(define-derived-mode arcit-revision-mode arcit-mode "Arcit-Revision"
  "Major mode for inspecting revisions in arcit."
  :group 'arcit)

(defvar-local arcit-buffer-pull-request nil
  "Holds the ID of the buffer's pull request, if present.")

(defclass arcit-revision-headers-section (arcit-section) nil)
(defclass arcit-revision-message-section (arcit-section) nil)
(defclass arcit-revision-diff-section (arcit-section) nil)

(defun arcit--insert-staff-link (login)
  "Inserts a clickable staff link"
  (insert-text-button (concat login "@")
                      'button-data login
                      'action (lambda (login)
                                (if (featurep 'ya-staff)
                                    (progn (declare-function yandex-show-staff-profile nil)
                                           (yandex-show-staff-profile login nil t))
                                  (browse-url (concat "https://staff.yandex-team.ru/" login))))))

(defun arcit--insert-ticket-links (tickets)
  (let ((first t))
    (seq-doseq (ticket tickets)
      (if first
          (setq first nil)
        (insert ", "))
      (insert-text-button ticket
                          'button-data (concat "https://st.yandex-team.ru/" ticket)
                          'action #'browse-url))))

(defun arcit-show-revision (commit)
  (interactive "sShow revision or commit: ")
  (let* ((revision (oref (arcit-sync-arc `("rev-parse" ,commit)) stdout))
         (arc-show-json-process
          (arcit-async-arc `("show" "--json" ,revision) 'json))

         ;; second arc show command that will retrieve diffs, but no structured data
         (arc-show-process (arcit-async-arc `("show" ,revision)))

         (buffer (get-buffer-create (format "arcit-rev: %s" (substring revision 0 8)) t))
         (inhibit-read-only t)
         (commit-data nil))
    (with-current-buffer buffer
      (erase-buffer)
      (arcit-revision-mode)
      (setq arcit--refresher (lambda () (arcit-show-revision commit)))

      (magit-insert-section (arcit-root-section)
        (magit-insert-heading "revision " commit)

        ;; draw metadata about the commit
        (magit-insert-section (arcit-revision-headers-section)
          (arcit-await-arc arc-show-json-process)
          ;; TODO: in which cases does `arc show' list *multiple* commits?
          (setq commit-data
                (seq-first
                 (map-elt (seq-first (oref arc-show-json-process stdout)) 'commits)))

          (seq-do
           (lambda (local-branch)
             (insert (propertize local-branch 'face 'arcit-branch-local) " "))
           (map-nested-elt commit-data '(branches local)))

          (seq-do
           (lambda (remote-branch)
             (insert (propertize (arcit--shorten-remote-branch remote-branch)
                                 'face 'arcit-branch-remote) " "))
           (map-nested-elt commit-data '(branches remote)))

          (insert (propertize revision 'face 'arcit-dimmed) "\n")

          (insert (string-pad "Author:" 10))
          (arcit--insert-staff-link (map-elt commit-data 'author))
          (insert "\n")

          (insert (string-pad "Date:" 10) (map-elt commit-data 'date)
                  " (" (russian-relative-time (parse-iso8601-time-string (map-elt commit-data 'date)))
                  ")" "\n")
          (insert (string-pad "Trunk:" 10)
                  (if-let (trunk-rev (map-elt commit-data 'revision))
                      (format "r%s" trunk-rev)
                    "not in trunk")
                  "\n")
          (when-let ((pr-id (map-nested-elt commit-data '(attributes pr.id))))
            (insert (string-pad "PR:" 10))
            (insert-text-button pr-id
                                'button-data (concat "https://a.yandex-team.ru/review/" pr-id)
                                'action #'browse-url)
            (insert "\n"))

          (when-let* ((tickets-raw (map-nested-elt commit-data '(attributes pr.tickets)))
                      (tickets (string-split tickets-raw "," t)))
            (insert (string-pad "Tickets:" 10))
            (arcit--insert-ticket-links tickets)
            (insert "\n")))
        (insert "\n")

        (magit-insert-section (arcit-revision-message-section)
          (insert (map-elt commit-data 'message) "\n"))

        (insert "\n")
        (magit-insert-section (arcit-revision-diff-section)
          (magit-insert-heading "File changes:")
          (if-let ((diffs (arcit--parse-file-diffs arc-show-process)))
              (dolist (diff diffs)
                (magit-insert-section (arcit-file-section)
                  (oset magit-insert-section--current path (car diff))
                  (magit-insert-heading (propertize (car diff) 'face 'bold))
                  (insert (cdr diff) "\n")))
            (insert "No diffs in this commit!\n")))

        (arcit--insert-bug-report-section))

      (goto-char (point-min)))

    (switch-to-buffer-other-window buffer)))

(provide 'arcit)
