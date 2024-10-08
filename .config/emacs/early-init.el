(setq inhibit-startup-message t
      frame-resize-pixelwise  t)

(tooltip-mode -1)
(blink-cursor-mode -1)

(setq use-dialog-box nil
      inhibit-startup-screen t)

(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))

(setenv "LC_CTYPE" "en_US.UTF-8")

(setq package-enable-at-startup nil)
(setq native-comp-async-report-warnings-errors nil)

(setq file-name-handler-alist nil)

(setq load-prefer-newer noninteractive)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)
