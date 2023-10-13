;;; doom-tokyonight-storm-theme.el --- adapted from equinusocio's Material themes -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: August 8, 2019 (7c7e871f2221)
;; Author: Brettm12345 <https://github.com/Brettm12345>
;; Maintainer:
;; Source: https://github.com/equinusocio/vsc-material-theme
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-tokyonight-storm-theme nil
  "Options for the `doom-tokyonight-storm' theme."
  :group 'doom-themes)

(defcustom doom-tokyonight-storm-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-tokyonight-storm-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-tokyonight-storm
  "A dark theme inspired by Material-Tokyonight-Storm"

  ;; name        default   256       16
  ((bg         '("#24283b"))
   (bg-alt     '("#1f2335"))
   (base0      '("#1c1f2b"))
   (base1      '("#1e212e"))
   (base2      '("#232635"))
   (base3      '("#3C435E"))
   (base4      '("#4E5579"))
   (base5      '("#676E95"))
   (base6      '("#697098"))
   (base7      '("#717CB4"))
   (base8      '("#A6Accd"))
   (fg         '("#c0caf5"))
   (fg-alt     '("#a9b1d6"))

   (grey base5)

   (red         '("#f7768e"))
   (orange      '("#ff9e64"))
   (green       '("#9ece6a"))
   (green1      '("#73daca"))
   (teal        '("#1abc9c"))
   (yellow      '("#e0af68"))
   (blue        '("#7aa2f7"))
   (dark-blue   '("#3d59a1"))
   (magenta     '("#bb9af7"))
   (violet      '("#9d7cd8"))
   (cyan        '("#7dcfff"))
   (dark-cyan   '("#2ac3de"))

   ;; face categories -- required for all themes
   (highlight      magenta)
   (vertical-bar   base2)
   (selection      blue)
   (builtin        blue)
   (comments       base5)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       magenta)
   (methods        blue)
   (operators      cyan)
   (punctuations   cyan)
   (type           dark-cyan)
   (strings        green)
   (variables      fg)
   (numbers        orange)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     base2)
   (modeline-bg-alt (doom-darken bg 0.01))
   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when doom-tokyonight-storm-padded-modeline
      (if (integerp doom-tokyonight-storm-padded-modeline) doom-tokyonight-storm-padded-modeline 4))))

  ;;;; Base theme face overrides
  ((lazy-highlight :background base4 :foreground fg :weight 'bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (tooltip :background (doom-darken bg-alt 0.2) :foreground fg)

   (font-lock-property-name-face :foreground green1)

   (cursor :background fg)

   (show-paren-match :foreground yellow)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored :foreground cyan)
   (dired-k-added    :foreground vc-added)
   ;;;; doom-modeline
   (doom-modeline-buffer-path       :foreground green :weight 'bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)
   ;;;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground yellow)
   (js2-object-property-access :foreground cyan)
   (js2-function-param         :foreground violet)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)
   ;;;; man <built-in>
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)
   ;;;; org <built-in>
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)

   (font-latex-sectioning-0-face :inherit 'default)
   (font-latex-sectioning-1-face :inherit 'default)
   (font-latex-sectioning-2-face :inherit 'default)
   (font-latex-sectioning-3-face :inherit 'default)
   (font-latex-sectioning-4-face :inherit 'default)
   (font-latex-sectioning-5-face :inherit 'default)
   (font-latex-sedate-face :inherit 'font-lock-keyword-face)
   (font-latex-warning-face :inherit 'font-lock-keyword-face)

   (dash-modeline-status :foreground bg :background blue)
   (dash-modeline-status-modified :foreground bg :background yellow)

   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)
   (markdown-link-face :foreground teal)
   (markdown-url-face :inherit 'underline :foreground fg)
   (markdown-markup-face :foreground fg)
   (jinx-misspelled :inherit 'warning :underline t)
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)))

;;; doom-tokyonight-storm-theme.el ends here
