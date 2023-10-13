;;; dash-agenda.el --- Dash agenda -*- lexical-binding: t -*-

;;; Code 
(require 'org)
(require 'cl-lib)
(require 'org-agenda)
(require 'org-duration)
(require 'calendar)


(defgroup dash-agenda nil
  "Dash agenda"
  :group 'dash)

(defgroup dash-agenda-faces nil
  "Dash agenda faces"
  :group 'dash-agenda)

(defvar dash-agenda--current-selection (current-time)
  "Current selected date")

(defvar dash-agenda--entry-overlay nil
  "Current selected date")

(defvar dash-agenda--busy-levels (list)
  "Cached list of (date busy-level) for internal use")

(defcustom dash-agenda-today-symbol  "•"
  "Symbol to show current day"
  :group 'dash-agenda)

(defcustom dash-agenda-deadline-symbol "• "
  "Symbol to show a deadline in calendar"
  :group 'dash-agenda)

(defcustom dash-agenda-separation "  "
  "Separation string between calenda and agenda entries"
  :group 'dash-agenda)

(defcustom dash-agenda-sort-function #'dash-agenda-default-sort-function
  "Function to sort a day's entries.
This function takes an entries list and returns the list in the desired order."
  :group 'dash-agenda)

(defcustom dash-agenda-filter-entry-predicate #'dash-agenda-filter-entry
  "Predicate to decide if entry will be shown in the dash-agenda buffer.
This function takes an entry and the selected date. Returns a value if the entry
should be shown, otherwise, returns nil."
  :group 'dash-agenda)

;; See https://material.io/design/color/the-color-system.html
(defvar dash-agenda-palettes
  '((red         . ("#FFEBEE" "#FFCDD2" "#EF9A9A" "#E57373" "#EF5350"
                    "#F44336" "#E53935" "#D32F2F" "#C62828" "#B71C1C"))
    (pink        . ("#FCE4EC" "#F8BBD0" "#F48FB1" "#F06292" "#EC407A"
                    "#E91E63" "#D81B60" "#C2185B" "#AD1457" "#880E4F"))
    (purple      . ("#F3E5F5" "#E1BEE7" "#CE93D8" "#BA68C8" "#AB47BC"
                    "#9C27B0" "#8E24AA" "#7B1FA2" "#6A1B9A" "#4A148C"))
    (deep-purple . ("#EDE7F6" "#D1C4E9" "#B39DDB" "#9575CD" "#7E57C2"
                    "#673AB7" "#5E35B1" "#512DA8" "#4527A0" "#311B92"))
    (indigo      . ("#E8EAF6" "#C5CAE9" "#9FA8DA" "#7986CB" "#5C6BC0"
                    "#3F51B5" "#3949AB" "#303F9F" "#283593" "#1A237E"))
    (blue        . ("#E3F2FD" "#BBDEFB" "#90CAF9" "#64B5F6" "#42A5F5"
                    "#2196F3" "#1E88E5" "#1976D2" "#1565C0" "#0D47A1"))
    (light-blue  . ("#E1F5FE" "#B3E5FC" "#81D4FA" "#4FC3F7" "#29B6F6"
                    "#03A9F4" "#039BE5" "#0288D1" "#0277BD" "#01579B"))
    (cyan        . ("#E0F7FA" "#B2EBF2" "#80DEEA" "#4DD0E1" "#26C6DA"
                    "#00BCD4" "#00ACC1" "#0097A7" "#00838F" "#006064"))
    (teal        . ("#E0F2F1" "#B2DFDB" "#80CBC4" "#4DB6AC" "#26A69A"
                    "#009688" "#00897B" "#00796B" "#00695C" "#004D40"))
    (green       . ("#E8F5E9" "#C8E6C9" "#A5D6A7" "#81C784" "#66BB6A"
                    "#4CAF50" "#43A047" "#388E3C" "#2E7D32" "#1B5E20"))
    (light-green . ("#F1F8E9" "#DCEDC8" "#C5E1A5" "#AED581" "#9CCC65"
                    "#8BC34A" "#7CB342" "#689F38" "#558B2F" "#33691E"))
    (lime        . ("#F9FBE7" "#F0F4C3" "#E6EE9C" "#DCE775" "#D4E157"
                    "#CDDC39" "#C0CA33" "#AFB42B" "#9E9D24" "#827717"))
    (yellow      . ("#FFFDE7" "#FFF9C4" "#FFF59D" "#FFF176" "#FFEE58"
                    "#FFEB3B" "#FDD835" "#FBC02D" "#F9A825" "#F57F17"))
    (amber       . ("#FFF8E1" "#FFECB3" "#FFE082" "#FFD54F" "#FFCA28"
                    "#FFC107" "#FFB300" "#FFA000" "#FF8F00" "#FF6F00"))
    (orange      . ("#FFF3E0" "#FFE0B2" "#FFCC80" "#FFB74D" "#FFA726"
                    "#FF9800" "#FB8C00" "#F57C00" "#EF6C00" "#E65100"))
    (deep-orange . ("#FBE9E7" "#FFCCBC" "#FFAB91" "#FF8A65" "#FF7043"
                    "#FF5722" "#F4511E" "#E64A19" "#D84315" "#BF360C"))
    (brown       . ("#EFEBE9" "#D7CCC8" "#BCAAA4" "#A1887F" "#8D6E63"
                    "#795548" "#6D4C41" "#5D4037" "#4E342E" "#3E2723"))
    (grey        . ("#FAFAFA" "#F5F5F5" "#EEEEEE" "#E0E0E0" "#BDBDBD"
                    "#9E9E9E" "#757575" "#616161" "#424242" "#212121"))
    (blue-grey   . ("#ECEFF1" "#CFD8DC" "#B0BEC5" "#90A4AE" "#78909C"
                    "#607D8B" "#546E7A" "#455A64" "#37474F" "#263238"))))


(defun dash-agenda-color-luminance (color)
  "Calculate the relative luminance of a color string (e.g. \"#ffaa00\", \"blue\").
Return a value between 0 and 1."
  (let* ((values (x-color-values color))
         (red (/ (car values) 256.0))
         (green (/ (cadr values) 256.0))
         (blue (/ (caddr values) 256.0)))
    (/ (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 255)))

(defcustom dash-agenda-palette 'amber
  "Background colors to use to highlight a day in calendar
  view according to busy level."
  :type `(choice (const red)    (const pink)  (const purple)      (const deep-purple)
                 (const indigo) (const blue)  (const light-blue)  (const cyan)
                 (const teal)   (const green) (const light-green) (const lime)
                 (const yellow) (const amber) (const orange)      (const deep-orange)
                 (const brown)  (const grey)  (const blue-grey))
  :group 'dash-agenda-faces)

(defface dash-agenda-default
  '((t :inherit default))
  "Default face (for casual day)"
  :group 'dash-agenda-faces)

(defface dash-agenda-selected
  '((t :inherit default :inverse-video t))
  "Face for the selected day"
  :group 'dash-agenda-faces)

(defface dash-agenda-selected-day
  '((t :inherit default :inverse-video t))
  "Face for the selected day"
  :group 'dash-agenda-faces)

(defface dash-agenda-time
  '((t :inherit font-lock-comment-face))
  "Time face"
  :group 'dash-agenda-faces)

(defface dash-agenda-current-day
  '((t :inherit bold))
  "Current day face"
  :group 'dash-agenda-faces)

(defface dash-agenda-weekend
  '((t :inherit font-lock-comment-face))
  "Weekend face"
  :group 'dash-agenda-faces)

(defface dash-agenda-outday
  '((t :inherit font-lock-comment-face))
  "Out day face, that is, day outside curent month."
  :group 'dash-agenda-faces)

(defface dash-agenda-day-name
  '((t :inherit font-lock-comment-face))
  "Day name face (on second line)"
  :group 'dash-agenda-faces)

(defface dash-agenda-month-name
  '((t :inherit bold))
  "Month name face (on first line)"
  :group 'dash-agenda-faces)

(defface dash-agenda-mouse
  '((t :inherit 'highlight ))
  "Mouse highlight face"
  :group 'dash-agenda-faces)

(defface dash-agenda-button
  '((t :inherit font-lock-comment-face))
  "Header button (left and right)"
  :group 'dash-agenda-faces)

(defun dash-agenda-date (year month day)
  "Return a date correspondng to DAY/MONTH/YEAR."
  (encode-time 0 0 0 day month year))

(defun dash-agenda-date-equal (date1 date2)
  "Check if DATE1 is equal to DATE2."
  (and (eq (dash-agenda-date-day date1)
           (dash-agenda-date-day date2))
       (eq (dash-agenda-date-month date1)
           (dash-agenda-date-month date2))
       (eq (dash-agenda-date-year date1)
           (dash-agenda-date-year date2))))

(defun dash-agenda-date-inc (date &optional days months years)
  "Return DATE + DAYS day & MONTH months & YEARS years"
  (let ((days (or days 0))
        (months (or months 0))
        (years (or years 0))
        (day (dash-agenda-date-day date))
        (month (dash-agenda-date-month date))
        (year (dash-agenda-date-year date)))
    (encode-time 0 0 0 (+ day days) (+ month months) (+ year years))))

(defun dash-agenda-date-dec (date &optional days months years)
  "Return DATE - DAYS day & MONTH months & YEARS years"
  (let ((days (or days 0))
        (months (or months 0))
        (years (or years 0)))
    (dash-agenda-date-inc date (- days) (- months) (- years))))

(defvar dash-agenda-hook nil
  "Normal hook run after agenda is build")

(defvar dash-agenda-update-hook nil
  "Normal hook run after agenda is updated")

(defvar dash-agenda-entry-edit-hook nil
  "Normal hook run after the editing buffer is shown")

(defun dash-agenda-date-day (date)
  "Return DATE day of month (1-31)."
  (nth 3 (decode-time date)))
                        
(defun dash-agenda-date-month (date)
  "Return DATE month number (1-12)."
  (nth 4 (decode-time date)))

(defun dash-agenda-date-year (date)
  "Return DATE year."
  (nth 5 (decode-time date)))

(defun dash-agenda-date-doy (date)
  "Return DATE day of year (1-366)."
  (string-to-number (format-time-string "%j" date)))

(defun dash-agenda-date-dow (date)
  "Return DATE day of week (0-6)."
  (nth 6 (decode-time date)))

(defun dash-agenda-date-day-name (date)
  "Return DATE full day name."
  (format-time-string "%A" date))

(defun dash-agenda-date-month-name (date)
  "Return DATE full month name."
  (format-time-string "%B" date))

(defun dash-agenda-date-is-today (date)
  "Check if DATE is today."
  (dash-agenda-date-equal (current-time) date))

(defun dash-agenda-date-today ()
  "Return today date."
  (current-time))

(defun dash-agenda-date-tomorrow ()
  "Return tomorrow date."
  (dash-agenda-date-inc (dash-agenda-date-today) 1 0 0))
  
(defun dash-agenda-yesterday ()
  "Return yesterday date."
  (dash-agenda-date-dec (dash-agenda-date-today) 1 0 0))

(defun dash-agenda-forward-day ()
  "Move to next day in calendar"
  
  (interactive)
  (setq dash-agenda--current-selection
        (dash-agenda-date-inc dash-agenda--current-selection 1))
  (dash-agenda-update))

(defun dash-agenda-backward-day ()
  "Move to previous day in calendar"
  
  (interactive)
  (setq dash-agenda--current-selection
        (dash-agenda-date-dec dash-agenda--current-selection 1))
  (dash-agenda-update))

(defun dash-agenda-forward-week ()
  "Move to next week in calendar or next entry when
entry-select-mode is active"
  
  (interactive)
  (dash-agenda-goto-next-entry)
  (setq dash-agenda--current-selection
        (dash-agenda-date-inc dash-agenda--current-selection 7))
  (dash-agenda-update))

(defun dash-agenda-backward-week ()
  "Move to previous week in calendar or previous entry when
entry-select-mode is active"
  
  (interactive)
  (dash-agenda-goto-prev-entry)
  (setq dash-agenda--current-selection
        (dash-agenda-date-dec dash-agenda--current-selection 7))
  (dash-agenda-update))

(defun dash-agenda-forward-month ()
  "Move to next month in calendar"
  
  (interactive)
  (setq dash-agenda--current-selection
        (dash-agenda-date-inc dash-agenda--current-selection 0 1))
  (dash-agenda-update))

(defun dash-agenda-backward-month ()
  "Move to previous month in calendar"
  
  (interactive)
  (setq dash-agenda--current-selection
        (dash-agenda-date-dec dash-agenda--current-selection 0 1))
  (dash-agenda-update))
  
(defun dash-agenda-forward-year ()
  "Move to next year in calendar"
  
  (interactive)
  (setq dash-agenda--current-selection
        (dash-agenda-date-inc dash-agenda--current-selection 0 0 1))
  (dash-agenda-update))
    
(defun dash-agenda-backward-year ()
  "Move to previous year in calendar"
  
  (interactive)
  (setq dash-agenda--current-selection
        (dash-agenda-date-dec dash-agenda--current-selection 0 0 1))
  (dash-agenda-update))

(defun dash-agenda-goto-today ()
  "Goto current day in calendar"
  
  (interactive)
  (setq dash-agenda--current-selection (dash-agenda-date-today))
  (dash-agenda-update))

(defun dash-agenda-goto (&optional date)
  "Goto given DATE (default today) in calendar"
  
  (interactive)
  (setq dash-agenda--current-selection (or date (dash-agenda-date-today)))
  (dash-agenda-update))

(define-minor-mode dash-agenda-mode
  "Minor mode for dash-agenda."
  :init nil
  :keymap `((,(kbd "<left>")    . dash-agenda-backward-day)
            (,(kbd "<right>")   . dash-agenda-forward-day)
            (,(kbd "<up>")      . dash-agenda-backward-week)
            (,(kbd "<down>")    . dash-agenda-forward-week)
            (,(kbd "<S-left>")  . dash-agenda-backward-month)
            (,(kbd "<S-right>") . dash-agenda-forward-month)
            (,(kbd "<S-up>")    . dash-agenda-backward-year)
            (,(kbd "<S-down>")  . dash-agenda-forward-year)
            (,(kbd "<return>")  . org-agenda-goto)
            (,(kbd "r")         . dash-agenda-refresh)
            (,(kbd ".")         . dash-agenda-goto-today)
            (,(kbd "t")         . dash-agenda-goto-today)
            (,(kbd "C-g")       . dash-agenda-kill)
            (,(kbd "q")         . dash-agenda-kill)
            ;; (,(kbd "<return>")  . dash-agenda-kill)
            (,(kbd "<escape>")  . dash-agenda-kill))

  (when dash-agenda-mode
    (setq buffer-read-only t)
    (setq cursor-type nil)))


(defun dash-agenda--center-string (string size)
  (let* ((padding (/ (- size (length string)) 2))
         (lpad (+ (length string) padding))
         (lformat (format "%%%ds" lpad))
         (rformat (format "%%%ds" (- size))))
    (format rformat (format lformat string))))


(defun dash-agenda--center-string-in-window (string)
  (dash-agenda--center-string string (window-body-width)))

(defun dash-agenda-select-window ()
  "Function to select where to show agenda. Default
behavior is to split vertically current window.

          (before)                        (after) 
+--------------------------+    +--------------------------+
|                          |    |                          |
|                          |    |                          |
|                          | -> |                          |
|                          |    +--------------------------+
|                          |    |    calendar /  agenda    |
+--------------------------+    +--------------------------+"

  (let* ((agenda-buffer "*dash-agenda*")
         (agenda-window (get-buffer-window agenda-buffer)))
    (or agenda-window (split-window nil -10 'below))))


(defun dash-agenda-filter-entry (entry &optional date)
  "Function to decide whether an entry is
displayed/counted. Default behavior is to select all entries."
  (let ((type (get-text-property 0 'type entry)))
    (and (not (string-equal type "upcoming-deadline"))
         (not (string-search ":CANCELLED:" entry)))))

(defun dash-agenda-default-sort-function (entry-1 entry-2)
  "Function to decide the order ENTRIES will be shown to the user.
Returns entries in `time-of-day' order."

  (let ((time-1 (get-text-property 0 'time-of-day entry-1))
        (time-2 (get-text-property 0 'time-of-day entry-2)))
    (cond ((not time-1) nil)
          ((not time-2) t)
          (t (< time-1 time-2)))))

(defun dash-agenda-format-entry (entry)
  "Function to display a specific (org) entry"

  (let* ((is-deadline (string-equal (get-text-property 0 'type entry) "deadline"))
         (org-marker (get-text-property 0 'org-marker entry))
         (text (propertize (get-text-property 0 'txt entry)
                           'org-marker org-marker))
         (text (replace-regexp-in-string ":.*:" "" text))
         (text (replace-regexp-in-string "TODO" "" text))
         (text (org-link-display-format text))
         (text (string-trim text))
         (time-of-day (get-text-property 0 'time-of-day entry))
         (hours (when time-of-day (floor (/ time-of-day 100))))
         (minutes (when time-of-day (% time-of-day 100)))
         (duration (or (get-text-property 0 'duration entry) 0))
         (text (if is-deadline
                   (concat
                    (propertize "[D]"'face 'org-imminent-deadline)
                    " "
                    (propertize text 'face 'default))
                 (propertize text 'face 'default)))
         (text (concat (propertize "-" 'face 'default) " " text)))
    (if hours
        (if duration
            (concat
             text
             " "
             (propertize
              (format "(%02d:%02d-%02d:%02d)"
                      hours
                      minutes
                      (+ hours (floor (/ duration 60)))
                      (+ minutes (% (floor duration) 60)))
              'face 'dash-agenda-time))
          (concat text " " (propertize (format "(%02d:%02d)" hours minutes) 'face 'dash-agenda-time)))
      text)))


;;;###autoload
(defun dash-agenda ()
  "Create windows & buffers associated with the agenda."

  (interactive)
  
  (let* ((agenda-buffer "*dash-agenda*")
         (agenda-window (dash-agenda-select-window)))
    (setq dash-agenda--entry-overlay nil)
    (select-window agenda-window)
    (switch-to-buffer agenda-buffer)
    (let ((message-log-max nil)
          (inhibit-message t))
      (toggle-truncate-lines 1))
    (set-window-dedicated-p agenda-window t)
    (set-window-margins agenda-window 2 2)
    (dash-agenda-mode t)
    (dash-agenda-update)
    (show-paren-local-mode -1)
    (run-hooks 'dash-agenda-hook)))

(defun dash-agenda-update ()
  "Update calendar and agenda according to selected date."
  
  (with-current-buffer "*dash-agenda*"
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-min))
      ;; (goto-char (point-min))

      (let* ((selected dash-agenda--current-selection)
             (day      (dash-agenda-date-day   selected))
             (month    (dash-agenda-date-month selected))
             (year     (dash-agenda-date-year  selected))
             (date     (list month day year))
             (today    (dash-agenda-date-today))
             (is-today (dash-agenda-date-is-today selected))
             (entries '()))

        ;; Header (literal date + holidays (if any))
        (forward-line)
        (end-of-line)
        (insert (propertize (format-time-string "%A %-e %B %Y" selected)
                            'face 'dash-agenda-current-day))
        (end-of-line)

        (if is-today
            (insert (propertize (format-time-string " (%H:%M)")
                                'face 'dash-agenda-time)))
        (forward-line 2)
        (end-of-line)
        (insert "\n"))

      (dash-agenda--populate-calendar)

      (insert "\n")
      (insert (propertize "Tasks:" 'face 'bold))
      (insert "\n")
      
      (dash-agenda--populate-agenda)
      (dash-agenda-goto-next-entry)
      (set-window-margins (get-buffer-window) 2 2))
    (run-hooks 'dash-agenda-update-hook)))

(defun dash-agenda-kill ()
  "Kill buffers and windows associated with the agenda."

  (interactive)
  (let* ((calendar-buffer "*dash-calendar*")
         (agenda-buffer "*dash-agenda*"))
    (if (get-buffer calendar-buffer)
        (kill-buffer calendar-buffer))
    (if (get-buffer agenda-buffer)
        (kill-buffer agenda-buffer))))

(defun dash-agenda-refresh ()
  "Reset the cache of busy levels."

  (interactive)
  (setq dash-agenda--busy-levels (list)))


(defun dash-agenda--has-deadline (date)
  "Check if a cached entry has a deadline"
    
  (let* ((day   (dash-agenda-date-day   date))
         (month (dash-agenda-date-month date))
         (year  (dash-agenda-date-year  date))
         (date  (list month day year))
         (entry (assoc date dash-agenda--busy-levels)))
    (when entry
      (nth 2 entry))))

(defun dash-agenda--busy-level (date)
  "Compute the busy level at a given date. This is done by
counting the number of timed entries. Computed levels are cached
for efficiency."
    
  (let* ((day   (dash-agenda-date-day   date))
         (month (dash-agenda-date-month date))
         (year  (dash-agenda-date-year  date))
         (date  (list month day year))
         (level 0)
         (deadline nil)
         (entry (assoc date dash-agenda--busy-levels)))
    (if entry
        (cadr entry)
      (progn
        (dolist (file (org-agenda-files))
          (dolist (entry (org-agenda-get-day-entries file date))
            (when (string-equal (get-text-property 0 'type entry) "deadline")
              (setq deadline t))
            (when (funcall dash-agenda-filter-entry-predicate entry date)
                (setq level (+ level 1)))))
        (add-to-list 'dash-agenda--busy-levels `(,date ,level ,deadline))
        level))))


(defun dash-agenda--populate-agenda ()
  "Populate the agenda according to current selected date."

  (let* ((selected dash-agenda--current-selection)
         (day      (dash-agenda-date-day   selected))
         (month    (dash-agenda-date-month selected))
         (year     (dash-agenda-date-year  selected))
         (date     (list month day year))
         (today    (dash-agenda-date-today))
         (is-today (dash-agenda-date-is-today selected))
         (entries '()))

    ;; Collect entries from agenda files.
    (dolist (file (org-agenda-files))
      (dolist (entry (org-agenda-get-day-entries file date))
        (if (funcall dash-agenda-filter-entry-predicate entry date)
            (add-to-list 'entries entry))))

    ;; Sort entries
    (setq entries (sort entries dash-agenda-sort-function))

    ;; Display entries
    (let ((limit (if (< (length entries) 1000) 1000 5)))
      (dolist (entry (cl-subseq entries 0 (min limit (length entries))))
        ;; (insert dash-agenda-separation)
        (let* ((org-marker (get-text-property 0 'org-marker entry))
               (entry-text (dash-agenda-format-entry entry))
               (entry-beg (point))
               (entry-end (+ (point) (length entry-text))))
          (insert (propertize (concat entry-text)
                              'overlay (cons entry-beg entry-end)))
          (insert "\n")
          ;; org-agenda-goto check at beginning of line for an org marker
          ;; We thus need to propertize the full line with the org-marker.
          (add-text-properties entry-beg entry-end
                               `(org-marker ,org-marker)))
        (forward-line)
        (end-of-line))
      (if (> (length entries) limit)
          (insert (concat dash-agenda-separation
                  (format "+%S non-displayed event(s)" (- (length entries) limit))))))
  
    (goto-char (point-min))))


(defun dash-agenda-select-entry ()
  (interactive)
  (let* ((match (text-property-search-forward 'overlay nil nil t)))
    (when match
      (let* ((bounds (prop-match-value match))
             (overlay (or dash-agenda--entry-overlay
                          (make-overlay (car bounds) (cdr bounds)))))
        (setq dash-agenda--entry-overlay overlay)
        (move-overlay overlay (car bounds) (cdr bounds))
        (overlay-put overlay 'face 'dash-agenda-selected)))))

(defun dash-agenda-goto-next-entry ()
  "Select next entry (when in entry select mode)"
  (interactive)
  (when-let* ((match (text-property-search-forward 'overlay nil nil t))
              (bounds (prop-match-value match))
              (overlay (or dash-agenda--entry-overlay
                           (make-overlay (car bounds) (cdr bounds)))))
    (setq dash-agenda--entry-overlay overlay)
    (move-overlay overlay (car bounds) (cdr bounds))
    (overlay-put overlay 'face 'dash-agenda-selected)))

(defun dash-agenda-goto-prev-entry ()
  "Select prev entry (when in entry select mode)"
  
  (interactive)
  (when-let* ((match (text-property-search-backward 'overlay nil nil t))
              (bounds (prop-match-value match))
              (overlay (or dash-agenda--entry-overlay
                           (make-overlay (car bounds) (cdr bounds)))))
    (setq dash-agenda--entry-overlay overlay)
    (move-overlay overlay (car bounds) (cdr bounds))
    (overlay-put overlay 'face 'dash-agenda-selected)))


(defun dash-agenda--populate-calendar ()
  "Populate the calendar according to the month of the current selected date."

  ;; Header with prev/next buttons
  ;; -----------------------------
  (let* ((selected dash-agenda--current-selection)
         (map-left (make-sparse-keymap))
         (map-right (make-sparse-keymap)))

    (define-key map-left (kbd "<down-mouse-1>") #'dash-agenda-backward-month)
    (define-key map-right (kbd "<down-mouse-1>") #'dash-agenda-forward-month)

    (insert "\n")
    (insert
     (dash-agenda--center-string-in-window
      (concat
       (propertize "<" 'face 'dash-agenda-button
                   'mouse-face 'dash-agenda-mouse
                   'help-echo "Previous month"
                   'keymap map-left)
       (propertize (dash-agenda--center-string
                    (format "%s %d"
                            (dash-agenda-date-month-name selected)
                            (dash-agenda-date-year selected)) 34)
                   'face 'dash-agenda-month-name)
       (propertize "> " 'face 'dash-agenda-button
                   'mouse-face 'dash-agenda-mouse
                   'help-echo "Next month"
                   'keymap map-right))))
    
    
    (insert "\n")

    (insert
     (dash-agenda--center-string-in-window
      (concat
       (propertize "Mo   Tu   We   Th   Fr   "
                   'face 'dash-agenda-day-name)
       (propertize "Sa   Su  "
                   'face 'dash-agenda-day-name))))
    (insert "\n"))
  
  ;; Body with navigation keymap
  ;; ---------------------------
  (let* ((selected dash-agenda--current-selection)
         (today    (dash-agenda-date-today))
         (day      (dash-agenda-date-day selected))
         (month    (dash-agenda-date-month selected))
         (year     (dash-agenda-date-year selected))
         (start    (dash-agenda-date year month 1))
         (dow      (mod (+ 6 (dash-agenda-date-dow start)) 7))
         (start    (dash-agenda-date-dec start dow)))

    (dotimes (row 6)
      (dotimes (col 7)
        (let* ((day (+ (* row 7) col))
               (date (dash-agenda-date-inc start day))

               ;; Slow
               (level (dash-agenda--busy-level date))
               (backgrounds (alist-get dash-agenda-palette dash-agenda-palettes))
               (level (min (length backgrounds) level))
               (background (nth (- level 1) backgrounds))
               (foreground (if (< (dash-agenda-color-luminance background) 0.5)
                               "white" "black"))
               (map (make-sparse-keymap))
               (is-today (dash-agenda-date-is-today date))
               (has-deadline (dash-agenda--has-deadline date))
               (is-selected (dash-agenda-date-equal date selected))
               (is-selected-today (and is-selected is-today))
               (is-outday (not (= (dash-agenda-date-month date) month)))
               (is-weekend (or (= (dash-agenda-date-dow date) 0)
                               (= (dash-agenda-date-dow date) 6)))
               (face (cond ;; (is-selected-today 'dash-agenda-selected-today)
                      ;;(is-selected       'dash-agenda-selected)
                           ((and is-selected (> level 0)) `(:foreground ,background :inherit ,'dash-agenda-selected-day))
                           (is-selected `(:inherit ,'dash-agenda-selected-day))
                           ;; (is-today          'dash-agenda-today)
                           (is-outday         'dash-agenda-outday)
                           ((> level 0)       `(:foreground ,background ))
                           (is-weekend        'dash-agenda-weekend)
                           (t                 'dash-agenda-default))))

          (define-key map (kbd "<down-mouse-1>")
            `(lambda() (interactive) (dash-agenda-goto ,date)))

          (when (eq col 0)
            (insert (make-string (max (/ (- (window-body-width) 36) 2) 0) ? )))

            (insert (propertize (format " %2d " (dash-agenda-date-day date))
                                'face face
                                'mouse-face (cond (is-selected-today 'dash-agenda-selected-today)
                                                  (is-selected       'dash-agenda-selected)
                                                  (t                 'dash-agenda-mouse))
                                'help-echo  (format-time-string "%A %-e %B %Y" date)
                                'keymap map))
            (if (< col 1000)
                ;; (insert (propertize (cond ((or has-deadline (> level 0))
                ;;                            dash-agenda-deadline-symbol)
                ;;                           (t "  "))
              (insert (propertize " " 'face face))  ;;                     'face face))
              )))
      (if (< row 1000) (insert "\n")))))

(provide 'dash-agenda)
;;; dash-agenda.el ends here
