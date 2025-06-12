;;; custom-faces.el --- contains custom faces I set to have more uniform themes. -*- lexical-binding: t -*-

;;; Commentary:


;;; Usage:

;;; Code:

;; (setq warning-minimum-level :emergency)

;; (defcustom modified-org-beautify-theme-use-box-hack 't
;;   "Use a 3 pixel box with the background color to add spacing.
;;   Note that this has a side effect that can make the theme look
;;   really bad under some circumstances."
;;   :type 'boolean)


(require 'color)
(require 'faces)
(defun my-format-color (color)
  "Change color.el name format COLOR to six digit hex."
  (concat "#" (substring color 1 3) (substring color 5 7) (substring color 9 11)))

(let* (;; (base-font-color (face-foreground 'default  nil 'default))
       ;; (background-color (face-background 'default nil 'default))
       ;; (headline `(:inherit default :foreground ,base-font-color))
       ;; ;; (primary-color (face-foreground 'mode-line nil))
       ;; (secondary-color (face-background 'secondary-selection nil 'region))
       ;; (org-highlights `(:foreground ,base-font-color :background ,secondary-color))
       (bg-color (if EMACS27+
                   (unless (string-equal (face-background 'default) "unspecified-bg")
                     (apply 'color-rgb-to-hex (append (color-name-to-rgb (face-background 'default)) '(2))))
				   (face-background 'default)))
       (fg-color (if EMACS27+
                   (unless (string-equal (face-background 'default) "unspecified-bg")
                     (apply 'color-rgb-to-hex (append (color-name-to-rgb (face-background 'default)) '(2))))
				   (face-foreground 'default)))
       )

;; (defun light-p (color)
;;   "Return t if COLOR is light."
;;   (>= (/ (- (apply #'max color)
;;              (apply #'min color))
;;           (apply #'max color))
;;       0.5))

;; (defun light-p (color)
;;   "Return t if COLOR is light, nil otherwise."
;;   (let ((rgb (color-values color)))
;;     (> (+ (* 0.299 (nth 0 rgb))
;;           (* 0.587 (nth 1 rgb))
;;           (* 0.114 (nth 2 rgb)))
;;        (/ 255.0 2))))

;;   (defun light-p (color)
;; 	"Checks if the color is light.
;;    color  The color to check.
;;    Returns non-nil if the color is light, nil if it is dark."
;; 	(let* ((hsl (apply #'color-rgb-to-hsl
;; 					   (color-name-to-rgb color)))
;;            (lightness (* (nth 2 hsl) 100)))
;;       (>= lightness 50)))

;; (defun dark-p (color)
;;   "Return t if COLOR is dark."
;;   (not (light-p color)))

(defun light-p (color)
  "Check if the COLOR is on the lighter side.
Returns non-nil if the color is light, nil if it is dark.
If COLOR is invalid, returns nil."
  (when-let ((rgb (color-name-to-rgb color)))
    (let* ((hsl (apply #'color-rgb-to-hsl rgb))
           (lightness (* (nth 2 hsl) 100)))
      (>= lightness 50))))

(defun dark-p (color)
  "Return t if COLOR is dark."
  (when color
    (not (light-p color))))

;; (defun light-p (color)
;;   "Checks if the color is light.
;;    color  The color to check.
;;    Returns non-nil if the color is light, nil if it is dark."
;;   (let ((components (color-name-to-rgb color)))
;;     (>= (/ (- (apply #'max components) (apply #'min components)) (apply #'max components)) 0.5)))

;; (defun dark-p (color)
;;   "Checks if the color is dark.
;;    color  The color to check.
;;    Returns non-nil if the color is light, nil if it is dark."
;;   (not light-p))


;; (defun yae-lighten-or-darken-color (color percent)
;;   "Lighten or darken COLOR by PERCENT."
;;   (let* ((color-rgb (color-name-to-rgb color))
;;          (r (car color-rgb))
;;          (g (cadr color-rgb))
;;          (b (caddr color-rgb))
;;          (new-r (+ r (* (/ percent 100.0) (- 255 r))))
;;          (new-g (+ g (* (/ percent 100.0) (- 255 g))))
;;          (new-b (+ b (* (/ percent 100.0) (- 255 b)))))
;;     (apply 'color-rgb-to-hex `(,new-r ,new-g ,new-b))))

;; (defun yae-lighten-or-darken-color (color percent)
;;   "Lighten or darken COLOR by PERCENT."
;;   (let* ((color-str (if (string-prefix-p "#" color)
;;                         color
;;                       (substring color 2)))
;;          (color-rgb (color-name-to-rgb color-str))
;;          (r (car color-rgb))
;;          (g (cadr color-rgb))
;;          (b (caddr color-rgb))
;;          (new-r (+ r (* (/ percent 100.0) (- 255 r))))
;;          (new-g (+ g (* (/ percent 100.0) (- 255 g))))
;;          (new-b (+ b (* (/ percent 100.0) (- 255 b)))))
;;     (if (= (length color-str) 6)
;;         (apply 'color-rgb-to-hex `(,new-r ,new-g ,new-b))
;;       (if (= (length color-str) 3)
;;           (substring color-str 1)
;;         (concat "#" (format "%02x" new-r) (format "%02x" new-g) (format "%02x" new-b))))))

;; (defun yae-lighten-or-darken-color (color percent)
;;   "Lighten or darken COLOR by PERCENT.
;; If the color is light and the percent is small, do not darken it to black."
;;   (let* ((color-str (if (string-prefix-p "#" color)
;; 						color
;; 					  (substring color 2)))
;; 		 (color-rgb (color-name-to-rgb color-str))
;; 		 (r (car color-rgb))
;; 		 (g (cadr color-rgb))
;; 		 (b (caddr color-rgb))
;; 		 (new-r (+ r (* (/ percent 100.0) (- 255 r))))
;; 		 (new-g (+ g (* (/ percent 100.0) (- 255 g))))
;; 		 (new-b (+ b (* (/ percent 100.0) (- 255 b)))))
;; 	(if (= (length color-str) 6)
;; 		(apply 'color-rgb-to-hex `(,new-r ,new-g ,new-b))
;; 	  (if (= (length color-str) 3)
;; 		  (substring color-str 1)
;; 		(let ((min-r 128)
;; 			  (min-g 128)
;; 			  (min-b 128))
;; 		  (cond ((>= r min-r) (concat "#" (format "%02x" new-r) (format "%02x" new-g) (format "%02x" new-b)))
;; 				((>= g min-g) (concat "#" (format "%02x" r) (format "%02x" new-g) (format "%02x" new-b)))
;; 				((>= b min-b) (concat "#" (format "%02x" r) (format "%02x" g) (format "%02x" new-b)))
;; 				(t color)))))))


;; (defun yae-lighten-or-darken-color (color percent)
;;   "Lighten or darken COLOR by PERCENT."
;;   (let* ((color-str (if (string-prefix-p "#" color)
;;                         color
;;                       (substring color 2)))
;;          (color-rgb (color-name-to-rgb color-str))
;;          (r (car color-rgb))
;;          (g (cadr color-rgb))
;;          (b (caddr color-rgb))
;;          (new-r (+ r (* (/ percent 100.0) (- 255 r))))
;;          (new-g (+ g (* (/ percent 100.0) (- 255 g))))
;;          (new-b (+ b (* (/ percent 100.0) (- 255 b)))))
;;     (if (= (length color-str) 6)
;;         (apply 'color-rgb-to-hex `(,new-r ,new-g ,new-b))
;;       (if (= (length color-str) 3)
;;           (substring color-str 1)
;;         (concat "#" (format "%02x" new-r) (format "%02x" new-g) (format "%02x" new-b))))))

;; (defun yae-lighten-or-darken-color (color percent)
;;   "Lighten or darken COLOR by PERCENT."
;;   (let* ((color-str (if (string-prefix-p "#" color)
;;                         color
;;                       (substring color 2)))
;;          (color-rgb (color-name-to-rgb color-str))
;;          (r (car color-rgb))
;;          (g (cadr color-rgb))
;;          (b (caddr color-rgb))
;;          (new-r (+ r (* (/ percent 100.0) (- 255 r))))
;;          (new-g (+ g (* (/ percent 100.0) (- 255 g))))
;;          (new-b (+ b (* (/ percent 100.0) (- 255 b)))))
;;         (apply 'color-rgb-to-hex `(,new-r ,new-g ,new-b))))


  (defun doomish-name-to-rgb (color)
    "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame)."
    (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
             for x in (tty-color-standard-values (downcase color))
             collect (/ x div)))


  (defun doom-color (name &optional type)
	"Retrieve a specific color named NAME (a symbol) from the current theme."
	(let ((colors (when (listp name)
                      name
					;; (cdr-safe (assq name doom-themes--colors))
					)))
      (and colors
           (cond ((listp colors)
                  (let ((i (or (plist-get '(256 1 16 2 8 3) type) 0)))
					(if (> i (1- (length colors)))
						(car (last colors))
                      (nth i colors))))
				 (t colors)))))
  
  (defun doomish-blend (color1 color2 alpha)
    "Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)"
    (when (and color1 color2)
      (cond ((and color1 color2 (symbolp color1) (symbolp color2))
             (doomish-blend (doom-color color1) (doom-color color2) alpha))

            ((or (listp color1) (listp color2))
             (cl-loop for x in color1
                      when (if (listp color2) (pop color2) color2)
                      collect (doomish-blend x it alpha)))

            ((and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
             (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
                    (cl-loop for it    in (doomish-name-to-rgb color1)
                             for other in (doomish-name-to-rgb color2)
                             collect (+ (* alpha it) (* other (- 1 alpha))))))

            (color1))))

  (defun doomish-darken (color alpha)
	"Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
	(cond ((and color (symbolp color))
           (doomish-darken (doom-color color) alpha))

          ((listp color)
           (cl-loop for c in color collect (doomish-darken c alpha)))

          ((doomish-blend color "#000000" (- 1 alpha)))))

  (defun doomish-lighten (color alpha)
	"Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
	(cond ((and color (symbolp color))
           (doomish-lighten (doom-color color) alpha))

          ((listp color)
           (cl-loop for c in color collect (doomish-lighten c alpha)))

          ((doomish-blend color "#FFFFFF" (- 1 alpha)))))

  ;; (unless (facep 'mode-line)
  ;; 	(defface mode-line '((t (:inherit variable-pitch
  ;; 									  :background ,(if (dark-p (color-name-to-rgb (face-background 'default)))
  ;; 													   (color-lighten-name (face-background 'default) 40)
  ;; 													 (color-darken-name (face-background 'default) 40)))))
  ;; 	  "Face used for the mode line." :group 'basic-faces))




  (custom-set-faces

   ;; ;;;;; spacemacs-boldening
   ;; `(font-lock-function-name-face ((t (:inherit bold))))
   ;; `(font-lock-keyword-face ((t (:inherit bold))))
   ;; `(font-lock-type-face ((t (:inherit bold))))
   ;; `(minibuffer-prompt ((t (:inherit bold))))
   ;; `(tooltip ((t (:bold nil :italic nil :underline nil))))

   ;; ;;;;; some fontlock italics
   ;; `(font-lock-comment-face ((t (:background nil :slant italic))))
   ;; ;;`(font-lock-constant-face ((t (:weight bold))))
   ;; `(font-lock-constant-face ((t (:weight normal :slant italic))))
   ;; `(font-lock-function-name-face ((t (:weight normal))))
   ;; `(font-lock-keyword-face ((t (:weight bold :slant italic))))
   ;; `(font-lock-type-face ((t (:slant italic)))) ;:weight bold))))

   ;; show-paren-match
   `(show-paren-match ((t (:inherit secondary-selection))))
   ;;;;; org
   ;; `(org-agenda-structure ((t (:inherit default ,@sans-font :height 2.0 :underline nil))))
   ;; `(org-level-8 ((t (:inherit 'outline-8
   ;;                             ;; :weight bold
   ;;                             :height 1.0
   ;;                             :box nil
   ;;                             :background ,(face-background 'default)
   ;;                             :overline nil))))
   ;; `(org-level-7 ((t (:inherit 'outline-7
   ;;                             ;; :weight bold
   ;;                             :height 1.0
   ;;                             :box nil
   ;;                             :background ,(face-background 'default)
   ;;                             :overline nil))))
   ;; `(org-level-6 ((t (:inherit 'outline-6
   ;;                             ;; :weight bold
   ;;                             :height 1.0
   ;;                             :box nil
   ;;                             :background ,(face-background 'default)
   ;;                             :overline nil))))
   ;; `(org-level-5 ((t (:inherit 'outline-5
   ;;                             ;; :weight bold
   ;;                             :height 1.0
   ;;                             :box nil
   ;;                             :background ,(face-background 'default)
   ;;                             :overline nil))))
   ;; `(org-level-4 ((t (:inherit 'outline-4
   ;;                             ;; :weight bold
   ;;                             :height 1.0
   ;;                             :box nil
   ;;                             :background ,(face-background 'default)
   ;;                             :overline nil))))
   ;; `(org-level-3 ((t (:inherit 'outline-3
   ;;                             ;; :weight bold
   ;;                             :height 1.0
   ;;                             :box nil
   ;;                             :background ,(face-background 'default)
   ;;                             :overline nil))))
   ;; `(org-level-2 ((t (:inherit 'outline-2
   ;;                             ;; :weight bold
   ;;                             :height 1.0
   ;;                             :box nil
   ;;                             :background ,(face-background 'default)
   ;;                             :overline nil))))
   ;; `(org-level-1 ((t (:inherit 'outline-1
   ;;                             ;; :weight bold
   ;;                             :height 1.0
   ;;                             :box nil
   ;;                             :background ,(face-background 'default)
   ;;                             :overline nil))))

   `(org-document-title ((t (:family "sans"
                                     :inherit org-level-1
                                     :height 1.5
                                     :underline nil
                                     :box nil))))

   `(org-block ((t (:inherit default :box nil))))
   `(org-block-begin-line ((t (;; :background ,(face-background 'highlight)
                               :inherit default
                               :foreground ,(face-foreground 'shadow) :underline nil :overline nil :italic t))))
   `(org-block-end-line ((t (;; :background ,(face-background 'highlight)
                             :inherit default
                             :foreground ,(face-foreground 'shadow) :overline nil :underline nil :italic t))))


   `(org-checkbox ((t (:foreground "#000000", :background "#93a1a1" ;; :box (:line-width -3 :color "#93a1a1" :style "released-button")
								   ))))

   `(org-headline-done ((t (:strike-through t))))
   `(org-done ((t (:strike-through t :underline t :overline t))))
   `(org-todo ((t (:inherit default :foreground ,(face-foreground 'default)
                            :underline t :overline t))))

   `(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))


   `(region
     ((((class color) (min-colors 257))
       (:background ,(if (dark-p bg-color)
                         (doomish-lighten bg-color 0.15)
                       (doomish-darken bg-color 0.15))))))

   ;;; modeline for color displays with more than 256 colors
   `(mode-line
     ((((class color) (min-colors 257))
       (:inherit variable-pitch
                 :background ,(if (dark-p bg-color)
                                  (doomish-lighten bg-color 0.15)
                                (doomish-darken bg-color 0.15))
                 :foreground ,(face-foreground 'default)
                 :box (:color ,(if (dark-p bg-color)
                                   (doomish-darken bg-color 0.20)
                                 (doomish-lighten bg-color 0.20))
                              :line-width 1)
                 :bold t))))

   `(mode-line-inactive
     ((((class color) (min-colors 257))
       (:inherit variable-pitch
                 :background ,(if (dark-p bg-color)
                                  (doomish-lighten bg-color 0.07)
                                (doomish-darken bg-color 0.07))
                 :foreground ,(face-foreground 'font-lock-comment-face)
                 :box (:color ,(if (dark-p bg-color)
                                   (doomish-darken bg-color 0.07)
                                 (doomish-lighten bg-color 0.07))
                              :line-width 1)
                 :italic t))))

   `(mode-line-buffer-id
     ((((class color) (min-colors 257))
       (:inherit variable-pitch
                 ;; :background ,(face-background 'mode-line)
                 :foreground ,(face-foreground 'link)
                 :bold t :height 1.0
                 :distant-foreground ,(face-background 'region)))))


   ;;header-line
   `(header-line ((t (:inherit default :background ,(face-background 'default) :box nil))))

   ;; menu & toolbar
  `(menu ((t (:background ,(face-background 'default) :foreground ,(face-foreground 'default) :italic nil :height 1.0 :bold t :underline nil))))
  `(tool-bar ((t (:background ,(face-background 'default) :foreground ,(face-foreground 'default) :italic nil :height 1.0 :bold t :underline nil))))

   ;;;;; whitespace-mode
   ;; `(trailing-whitespace ((t nil)))
   `(whitespace-empty ((t nil)))
   `(whitespace-line ((t nil)))
   `(whitespace-newline ((t (:foreground ,(face-background 'default) ;; :background ,(face-background 'default)
                                         ))))
   `(whitespace-newline-mark ((t (:foreground ,(face-background 'default) :background ,(face-background 'default)))))
   `(whitespace-space ((t (:foreground ,(face-background 'default) :background ,(face-background 'default)))))
   `(whitespace-spaces ((t (:foreground ,(face-background 'default) :background ,(face-background 'default)))))
   `(whitespace-space-mark ((t (:foreground ,(face-background 'default) :background ,(face-background 'default)))))
   `(whitespace-tabs ((t (:foreground ,(face-background 'default) :background ,(face-background 'default)))))
   `(whitespace-tab ((t (:foreground ,(face-background 'default) :background ,(face-background 'default)))))
   ;; `(whitespace-trailing ((t nil)))
   ;; `(whitespace-space-after-tab ((t nil)))
   ;; `(whitespace-space-before-tab ((t nil)))
   `(whitespace-empty ((t (:foreground ,(face-background 'default) :background ,(face-background 'default)))))
   `(whitespace-trailing ((t (:foreground ,(face-background 'default) :background ,(face-background 'default)))))
   `(whitespace-indentation ((t (:foreground ,(face-background 'default) :background ,(face-background 'default)))))
   `(whitespace-space-before-tab ((t (:foreground ,(face-background 'default) :background ,(face-background 'default)))))

   `(icomplete-selected-match
	 ((t (:foreground ,(doomish-blend (face-foreground 'link) (face-foreground 'default) 1.0)
		  :underline t :overline nil :bold t :italic nil :height 1.05))))
   `(completions-common-part
	 ((t (:foreground ,(doomish-blend (face-foreground 'link) (face-foreground 'default) 0.7)
		  :height 1.0 :bold nil :italic nil :bold t))))
   `(completions-first-difference
	 ((t (:foreground ,(doomish-blend (face-foreground 'font-lock-string-face) (face-background 'default) 0.6)
		  :italic nil :height 1.0 :bold t :underline nil))))

   ;; ;;;;; dired-subtree

   `(dired-subtree-depth-1-face ((t (:background nil)))) ;; :foreground ,(face-foreground 'default)))))
   `(dired-subtree-depth-2-face ((t (:background nil)))) ;; :foreground ,(face-foreground 'default)))))
   `(dired-subtree-depth-3-face ((t (:background nil)))) ;; :foreground ,(face-foreground 'default)))))
   `(dired-subtree-depth-4-face ((t (:background nil)))) ;; :foreground ,(face-foreground 'default)))))
   `(dired-subtree-depth-5-face ((t (:background nil)))) ;; :foreground ,(face-foreground 'default)))))
   `(dired-subtree-depth-6-face ((t (:background nil)))) ;; :foreground ,(face-foreground 'default)))))


   `(tab-line ((t (:inherit mode-line-inactive ;; :background ,(face-background 'default) :foreground ,(face-foreground 'default) 
                            :height 1.1 ;; :overline t
                            ;; :box (:color ,(face-foreground 'default) :line-width 1)
                            ))))
   `(tab-line-tab ((t (:inherit variable-pitch :background ,(face-background 'default) :foreground ,(face-foreground 'default) :height 1.0
                                ;; :inverse-video t
                                ;; :underline t
                                ;; :box (:color ,(face-foreground 'default) :line-width 1)
                                ))))
   `(tab-line-tab-inactive ((t (:inherit mode-line-inactive
                                         ;; :box (:color ,(face-foreground 'default) :line-width 1)
                                         ))))

   `(tab-line-tab-current ((t (:inherit variable-pitch :background ,(face-background 'default) :foreground ,(face-foreground 'default) :height 1.0 ;; :inverse-video t
                                        ;; :underline t
                                        ;; :box (:color ,(face-foreground 'default) :line-tawidth 1)
                                        ))))
   `(tab-line-highlight ((t (:inherit variable-pitch :background ,(face-background 'region) :foreground ,(face-background 'default) :height 1.0 ;; :inverse-video t
                                      ;; :underline t
                                      ;; :box (:color ,(face-foreground 'default) :line-width 1)
                                      ))))

   `(tab-bar ((t (:inherit mode-line-inactive ;; :background ,(face-background 'default) :foreground ,(face-foreground 'default) 
                           :height 1.1 ;; :overline t
                           ;; :box (:color ,(face-foreground 'default) :line-width 1)
                           ))))
   `(tab-bar-tab ((t (:inherit variable-pitch :background ,(face-background 'default) :foreground ,(face-foreground 'default) :height 1.0
                               ;; :inverse-video t
                               ;; :underline t
                               ;; :box (:color ,(face-foreground 'default) :line-width 1)
                               ))))
   `(tab-bar-inactive ((t (:inherit mode-line-inactive
                                    ;; :box (:color ,(face-foreground 'default) :line-width 1)
                                    ))))


   ;;;;; dashboard
   `(dashboard-items-face ((t (:underline nil :height 0.9 ))))
   `(dashboard-no-items-face ((t (:underline nil :height 1.0 :foreground ,(face-foreground font-lock-comment-face) ))))
   `(dashboard-text-banner-face ((t (:underline nil :height 1.2))))

   ;;;; diredfl

   `(diredfl-autofile-name          ((t (:background nil :foreground ,(face-foreground 'border)))))
   `(diredfl-compressed-file-name   ((t (:background nil :foreground ,(face-foreground 'cursor)))))
   `(diredfl-compressed-file-suffix ((t (:background nil :foreground ,(doomish-blend (face-foreground 'default) (face-background 'default) 0.4)))))
   `(diredfl-date-time              ((t (:background nil :foreground ,(doomish-blend (face-foreground 'default) (face-background 'default) 0.4) :weight light :slant italic))))
   `(diredfl-deletion               ((t (:background nil :foreground ,(face-foreground 'success) :weight bold))))
   `(diredfl-deletion-file-name     ((t (:background nil :foreground ,(face-foreground 'success)))))
   `(diredfl-dir-heading            ((t (:background nil :foreground ,(face-foreground 'link) :weight bold))))
   `(diredfl-dir-name               ((t (:background nil :foreground ,(face-foreground 'link) :weight bold))))
   `(diredfl-dir-priv               ((t (:background nil :foreground ,(face-foreground 'link)))))
   `(diredfl-exec-priv              ((t (:background nil :foreground ,(face-foreground 'warning)))))
   `(diredfl-executable-tag         ((t (:background nil :foreground ,(face-foreground 'warning)))))
   `(diredfl-file-name              ((t (:background nil :foreground ,(face-foreground 'default) :weight bold))))
   `(diredfl-file-suffix            ((t (:background nil :foreground ,(doomish-blend (face-foreground 'default) (face-background 'default) 0.4)))))
   `(diredfl-flag-mark              ((t (:background nil :foreground ,(face-foreground 'shadow) :weight bold))))
   `(diredfl-flag-mark-line         ((t (:background ,(doomish-blend "#ffff00" (face-background 'default) 0.1)))))
   `(diredfl-ignored-file-name      ((t (:background nil :foreground ,(face-foreground 'font-lock-comment-face)))))
   `(diredfl-link-priv              ((t (:background nil :foreground ,(face-foreground 'trailing-whitespace)))))
   `(diredfl-no-priv                ((t (:background nil :foreground ,(face-foreground 'default)))))
   ;; `(diredfl-number                 ((t (:background nil :foreground ,(face-foreground 'header-line)))))
   `(diredfl-other-priv             ((t (:background nil :foreground ,(face-foreground 'highlight)))))
   `(diredfl-rare-priv              ((t (:background nil :foreground ,(face-foreground 'default)))))
   `(diredfl-read-priv              ((t (:background nil :foreground ,(face-foreground 'font-lock-regexp-grouping-construct)))))
   `(diredfl-symlink                ((t (:background nil :foreground ,(face-foreground 'trailing-whitespace)))))
   `(diredfl-tagged-autofile-name   ((t (:background nil :foreground ,(face-foreground 'region)))))
   `(diredfl-write-priv             ((t (:background nil :foreground ,(face-foreground 'font-lock-variable-name-face)))))

   `(highlight-indent-guides-odd-face       ((t (:background ,(face-background 'default) :foreground ,(face-foreground 'region)))))
   `(highlight-indent-guides-even-face      ((t (:background ,(face-background 'default) :foreground ,(face-foreground 'font-lock-comment-face)))))
   `(highlight-indent-guides-character-face ((t (:background ,(face-background 'default) :foreground ,(face-foreground 'font-lock-comment-face)))))

   `(fringe ((t (:background ,(face-background 'default) :foreground ,(face-foreground 'font-lock-comment-face)))))

	;;;;; display-line-number-mode
   `(line-number ((t (:background ,(face-background 'default) :foreground ,(face-foreground 'font-lock-comment-face)))))
   `(line-number-current-line ((t (:background ,(face-background 'default) :foreground ,(face-foreground 'font-lock-comment-face) :weight extra-bold ))))
    ;; `(line-number ((t (:inherit linum :weight thin :inherit line-number :weight thin :underline nil :height 0.8 :italic t))))
    ;;;;; linum-mode
   ;; `(linum ((t (:inherit line-number :weight thin :height 0.8 :underline nil :font ,(face-font 'highlight) :italic t))))
   `(linum ((t (:background ,(face-background 'default) :foreground ,(face-foreground 'font-lock-comment-face)))))
   `(linum-relative-current-face ((t (:inherit linum :height 1.0 :weight extra-bold :italic nil))))

   `(hl-line
     ((((class color) (min-colors 257))
       (:underline nil :background ,(if (dark-p bg-color)  (doomish-lighten bg-color 0.05) (doomish-darken bg-color 0.05) :extend t )))))

   `(my/org-comment-heading
     ((t (:slant italic :foreground ,(face-background 'region)))))

   ))

(setq warning-minimum-level :warning)

(provide 'custom-faces)
;;; custom_faces.el ends here
