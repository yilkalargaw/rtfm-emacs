;;; transients.el --- independent transient menus

(require 'transient)

(transient-define-prefix transient-zoom ()
  "Zoom"
  ["Zoom Controls"
   ("g" "Zoom In" (lambda () (interactive) (text-scale-increase 1)) :transient t)
   ("l" "Zoom Out" (lambda () (interactive) (text-scale-decrease 1)) :transient t)
   ("o" "Reset Zoom" (lambda () (interactive) (text-scale-adjust 0)) :transient t)
   ("q" "Quit" transient-quit-one)])

;; (transient-define-prefix transient-ibuffer-main ()
;;   "Ibuffer"
;;   ["Navigation"
;;    ("j" "Down" ibuffer-forward-line :transient t)
;;    ("k" "Up" ibuffer-backward-line :transient t)
;;    ("RET" "Visit" ibuffer-visit-buffer)
;;    ("o" "Visit (other win)" ibuffer-visit-buffer-other-window)]

;;   ["Mark"
;;    ("m" "Mark" ibuffer-mark-forward :transient t)
;;    ("u" "Unmark" ibuffer-unmark-forward :transient t)
;;    ("*" "Mark Submenu" transient-ibuffer-mark)]

;;   ["Actions"
;;    ("D" "Delete" ibuffer-do-delete :transient t)
;;    ("S" "Save" ibuffer-do-save :transient t)
;;    ("a" "All Actions" transient-ibuffer-action)]

;;   ["View"
;;    ("g" "Refresh" ibuffer-update :transient t)
;;    ("s" "Sort" transient-ibuffer-sort)
;;    ("/" "Filter" transient-ibuffer-filter)
;;    ("q" "Quit" quit-window)])


;; (transient-define-prefix transient-ibuffer-mark ()
;;   "Ibuffer Mark"
;;   ["Mark"
;;    ("*" "Unmark All" ibuffer-unmark-all)
;;    ("M" "By Mode" ibuffer-mark-by-mode)
;;    ("m" "Modified" ibuffer-mark-modified-buffers)
;;    ("u" "Unsaved" ibuffer-mark-unsaved-buffers)
;;    ("s" "Special" ibuffer-mark-special-buffers)
;;    ("r" "Read-only" ibuffer-mark-read-only-buffers)
;;    ("/" "Dired" ibuffer-mark-dired-buffers)
;;    ("e" "Dissociated" ibuffer-mark-dissociated-buffers)
;;    ("h" "Help" ibuffer-mark-help-buffers)
;;    ("z" "Compressed" ibuffer-mark-compressed-file-buffers)
;;    ("b" "Back" transient-ibuffer-main)])


;; (transient-define-prefix transient-ibuffer-action ()
;;   "Ibuffer Actions"
;;   ["Actions"
;;    ("A" "View" ibuffer-do-view)
;;    ("E" "Eval" ibuffer-do-eval)
;;    ("F" "Shell Cmd File" ibuffer-do-shell-command-file)
;;    ("I" "Query Replace Regexp" ibuffer-do-query-replace-regexp)
;;    ("H" "View Other Frame" ibuffer-do-view-other-frame)
;;    ("N" "Shell Pipe Replace" ibuffer-do-shell-command-pipe-replace)
;;    ("M" "Toggle Modified" ibuffer-do-toggle-modified)
;;    ("O" "Occur" ibuffer-do-occur)
;;    ("P" "Print" ibuffer-do-print)
;;    ("Q" "Query Replace" ibuffer-do-query-replace)
;;    ("R" "Rename Uniquely" ibuffer-do-rename-uniquely)
;;    ("T" "Toggle RO" ibuffer-do-toggle-read-only)
;;    ("U" "Replace Regexp" ibuffer-do-replace-regexp)
;;    ("V" "Revert" ibuffer-do-revert)
;;    ("W" "View + Eval" ibuffer-do-view-and-eval)
;;    ("X" "Shell Pipe" ibuffer-do-shell-command-pipe)
;;    ("b" "Back" transient-ibuffer-main)])

;; (transient-define-prefix transient-ibuffer-sort ()
;;   "Ibuffer Sort"
;;   ["Sort"
;;    ("i" "Invert" ibuffer-invert-sorting)
;;    ("a" "Alphabetic" ibuffer-do-sort-by-alphabetic)
;;    ("v" "Recently Used" ibuffer-do-sort-by-recency)
;;    ("s" "Size" ibuffer-do-sort-by-size)
;;    ("f" "Filename" ibuffer-do-sort-by-filename/process)
;;    ("m" "Major Mode" ibuffer-do-sort-by-major-mode)
;;    ("b" "Back" transient-ibuffer-main)])


;; (transient-define-prefix transient-ibuffer-filter ()
;;   "Ibuffer Filter"
;;   ["Filter"
;;    ("m" "Used Mode" ibuffer-filter-by-used-mode)
;;    ("M" "Derived Mode" ibuffer-filter-by-derived-mode)
;;    ("n" "Name" ibuffer-filter-by-name)
;;    ("c" "Content" ibuffer-filter-by-content)
;;    ("e" "Predicate" ibuffer-filter-by-predicate)
;;    ("f" "Filename" ibuffer-filter-by-filename)
;;    (">" "Size >" ibuffer-filter-by-size-gt)
;;    ("<" "Size <" ibuffer-filter-by-size-lt)
;;    ("/" "Disable" ibuffer-filter-disable)
;;    ("b" "Back" transient-ibuffer-main)])


;; (transient-define-prefix transient-avy ()
;;   "Avy"
;;   ["Character Navigation"
;;    ("c" "Char" avy-goto-char)
;;    ("C" "Char-2" avy-goto-char-2)
;;    ("t" "Char-Timer" avy-goto-char-timer)]

;;   ["Other Targets"
;;    ("w" "Word" avy-goto-word-1)
;;    ("s" "Subword" avy-goto-subword-1)
;;    ("l" "Line" avy-goto-line)
;;    ("q" "Quit" transient-quit-one)])



;; (transient-define-prefix transient-transpose ()
;;   "Transpose"
;;   ["Basic"
;;    ("c" "Chars" transpose-chars :transient t)
;;    ("w" "Words" transpose-words :transient t)
;;    ("l" "Lines" transpose-lines :transient t)
;;    ("s" "Sentences" transpose-sentences :transient t)
;;    ("p" "Paragraphs" transpose-paragraphs :transient t)]

;;   ["Sexps & Org"
;;    ("x" "Sexps" transpose-sexps :transient t)
;;    ("o" "Org Words" org-transpose-words :transient t)
;;    ("e" "Org Elements" org-transpose-elements :transient t)
;;    ("t" "Org Table" org-table-transpose-table-at-point :transient t)]

;;   [""
;;    ("q" "Quit" transient-quit-one)])


(transient-define-prefix transient-launcher ()
  "Launch Tools"
  [[""
    ("m" "Man" woman)
    ("d" "DuckDuckGo" (lambda () (interactive) (eww-browse-url "https://duckduckgo.com/")))
    ("f" "Open File in EWW" eww-open-file)
    ("s" "Shell" shell)
    ("e" "Eshell" eshell)
    ("t" "Scratch/Temp" (lambda () (interactive) (switch-to-buffer "*scratch*")))]
   ["" ("q" "Quit" transient-quit-one)]])


(transient-define-prefix transient-toggle ()
  "Toggle settings"
  ["Toggles"
   ("a" "Abbrev Mode" abbrev-mode)
   ("d" "Debug on Error" toggle-debug-on-error)
   ("f" "Auto Fill Mode" auto-fill-mode)
   ("t" "Truncate Lines" toggle-truncate-lines)
   ("w" "Whitespace Mode" whitespace-mode)
   ("q" "Quit" transient-quit-one)])


(transient-define-prefix transient-hide-show ()
  "Hide/Show code blocks"
  ["Hide/Show"
   ("h" "Hide Block" hs-hide-block)
   ("s" "Show Block" hs-show-block)
   ("H" "Hide All" hs-hide-all)
   ("S" "Show All" hs-show-all)
   ("q" "Quit" transient-quit-one)])


;; Define a regular keymap as the prefix map
(define-prefix-command 'transient-map)

;; Bind your transients to specific keys inside the `transient-map`

;; (define-key transient-map (kbd "m") 'transient-multiple-cursors) ;; Replace hydra-multiple-cursors
;; (define-key transient-map (kbd "a") 'transient-avy)               ;; Replace hydra-avy
;; (define-key transient-map (kbd "n") 'transient-move)              ;; Replace hydra-move
;; (define-key transient-map (kbd "r") 'transient-rectangle)          ;; Replace hydra-rectangle
;; (define-key transient-map (kbd "w") 'transient-window)             ;; Replace hydra-window
;; (define-key transient-map (kbd "p") 'transient-projectile)         ;; Replace hydra-projectile
;; (define-key transient-map (kbd "s") 'transient-learn-sp)           ;; Replace hydra-learn-sp
;; (define-key transient-map (kbd "t") 'transient-transpose)          ;; Replace hydra-transpose
(define-key transient-map (kbd "z") 'transient-zoom)          ;; Replace hydra-transpose
(define-key transient-map (kbd "l") 'transient-launcher)           ;; Replace hydra-launcher
(define-key transient-map (kbd "t") 'transient-toggle)           ;; Replace hydra-launcher

;; ;; New Transient Toggles (Replacing hydra-toggle)
;; (define-key transient-map (kbd "t") 'transient-toggle)             ;; Replace hydra-toggle
;; (define-key transient-map (kbd "h") 'transient-hide-show)          ;; Replace hydra-hide-show

;; ;; Bind the `transient-map` to a global key
(global-set-key (kbd "C-c h") 'transient-map)
