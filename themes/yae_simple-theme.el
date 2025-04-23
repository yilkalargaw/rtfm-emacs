;;; yae_simple

;; Copyright (C) 2024

;; Author: Yilkal Argaw

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;;###theme-autoload

(defgroup yae-simple-theme nil
  "Custom theme settings for YAE."
  :group 'faces)

(defcustom yae-color-palettes
  '((palenight
     .   ((yae-ansi-black . "#292d3e")
          (yae-ansi-blue . "#82aaff")
          (yae-ansi-brightblack . "#676e95")
          (yae-ansi-brightblue . "#82aaff")
          (yae-ansi-brightcyan . "#89ddff")
          (yae-ansi-brightgreen . "#c3e88d")
          (yae-ansi-brightmagenta . "#c792ea")
          (yae-ansi-brightred . "#f07178")
          (yae-ansi-brightwhite . "#ffffff")
          (yae-ansi-brightyellow . "#ffcb6b")
          (yae-ansi-cyan . "#89ddff")
          (yae-ansi-green . "#c3e88d")
          (yae-ansi-magenta . "#c792ea")
          (yae-ansi-red . "#f07178")
          (yae-ansi-white . "#959dcb")
          (yae-ansi-yellow . "#ffcb6b")))

    (harmonic-dark
     .   ((yae-ansi-black . "#0B1C2C")
          (yae-ansi-blue . "#8B56BF")
          (yae-ansi-brightblack . "#627E99")
          (yae-ansi-brightblue . "#8B56BF")
          (yae-ansi-brightcyan . "#568BBF")
          (yae-ansi-brightgreen . "#56BF8B")
          (yae-ansi-brightmagenta . "#BF568B")
          (yae-ansi-brightred . "#BF8B56")
          (yae-ansi-brightwhite . "#F7F9FB")
          (yae-ansi-brightyellow . "#8BBF56")
          (yae-ansi-cyan . "#568BBF")
          (yae-ansi-green . "#56BF8B")
          (yae-ansi-magenta . "#BF568B")
          (yae-ansi-red . "#BF8B56")
          (yae-ansi-white . "#CBD6E2")
          (yae-ansi-yellow . "#8BBF56")))

    (grayscale-dark
     . ((yae-ansi-black . "#101010")
        (yae-ansi-blue . "#686868")
        (yae-ansi-brightblack . "#525252")
        (yae-ansi-brightblue . "#686868")
        (yae-ansi-brightcyan . "#868686")
        (yae-ansi-brightgreen . "#8e8e8e")
        (yae-ansi-brightmagenta . "#747474")
        (yae-ansi-brightred . "#7c7c7c")
        (yae-ansi-brightwhite . "#f7f7f7")
        (yae-ansi-brightyellow . "#a0a0a0")
        (yae-ansi-cyan . "#868686")
        (yae-ansi-green . "#8e8e8e")
        (yae-ansi-magenta . "#747474")
        (yae-ansi-red . "#7c7c7c")
        (yae-ansi-white . "#b9b9b9")
        (yae-ansi-yellow . "#a0a0a0")))

    (brewer
     . ((yae-ansi-black . "#0c0d0e")
        (yae-ansi-blue . "#3182bd")
        (yae-ansi-brightblack . "#737475")
        (yae-ansi-brightblue . "#3182bd")
        (yae-ansi-brightcyan . "#80b1d3")
        (yae-ansi-brightgreen . "#31a354")
        (yae-ansi-brightmagenta . "#756bb1")
        (yae-ansi-brightred . "#e31a1c")
        (yae-ansi-brightwhite . "#fcfdfe")
        (yae-ansi-brightyellow . "#dca060")
        (yae-ansi-cyan . "#80b1d3")
        (yae-ansi-green . "#31a354")
        (yae-ansi-magenta . "#756bb1")
        (yae-ansi-red . "#e31a1c")
        (yae-ansi-white . "#b7b8b9")
        (yae-ansi-yellow . "#dca060")))

    (grayscale-light
     . ((yae-ansi-black . "#f7f7f7")
        (yae-ansi-blue . "#686868")
        (yae-ansi-brightblack . "#ababab")
        (yae-ansi-brightblue . "#686868")
        (yae-ansi-brightcyan . "#868686")
        (yae-ansi-brightgreen . "#8e8e8e")
        (yae-ansi-brightmagenta . "#747474")
        (yae-ansi-brightred . "#7c7c7c")
        (yae-ansi-brightwhite . "#101010")
        (yae-ansi-brightyellow . "#a0a0a0")
        (yae-ansi-cyan . "#868686")
        (yae-ansi-green . "#8e8e8e")
        (yae-ansi-magenta . "#747474")
        (yae-ansi-red . "#7c7c7c")
        (yae-ansi-white . "#464646")
        (yae-ansi-yellow . "#a0a0a0")))

    (nord
     . ((yae-ansi-black . "#2e3440")
        (yae-ansi-blue . "#ebcb8b")
        (yae-ansi-brightblack . "#4c566a")
        (yae-ansi-brightblue . "#ebcb8b")
        (yae-ansi-brightcyan . "#d08770")
        (yae-ansi-brightgreen . "#bf616a")
        (yae-ansi-brightmagenta . "#a3be8c")
        (yae-ansi-brightred . "#88c0d0")
        (yae-ansi-brightwhite . "#8fbcbb")
        (yae-ansi-brightyellow . "#5e81ac")
        (yae-ansi-cyan . "#d08770")
        (yae-ansi-green . "#bf616a")
        (yae-ansi-magenta . "#a3be8c")
        (yae-ansi-red . "#88c0d0")
        (yae-ansi-white . "#e5e9f0")
        (yae-ansi-yellow . "#5e81ac")))

    (solarized-dark
     . ((yae-ansi-black . "#002b36")
        (yae-ansi-blue . "#268bd2")
        (yae-ansi-brightblack . "#657b83")
        (yae-ansi-brightblue . "#268bd2")
        (yae-ansi-brightcyan . "#2aa198")
        (yae-ansi-brightgreen . "#859900")
        (yae-ansi-brightmagenta . "#6c71c4")
        (yae-ansi-brightred . "#dc322f")
        (yae-ansi-brightwhite . "#fdf6e3")
        (yae-ansi-brightyellow . "#b58900")
        (yae-ansi-cyan . "#2aa198")
        (yae-ansi-green . "#859900")
        (yae-ansi-magenta . "#6c71c4")
        (yae-ansi-red . "#dc322f")
        (yae-ansi-white . "#93a1a1")
        (yae-ansi-yellow . "#b58900")))

    (solarized-light
     . ((yae-ansi-black . "#fdf6e3")
        (yae-ansi-blue . "#268bd2")
        (yae-ansi-brightblack . "#839496")
        (yae-ansi-brightblue . "#268bd2")
        (yae-ansi-brightcyan . "#2aa198")
        (yae-ansi-brightgreen . "#859900")
        (yae-ansi-brightmagenta . "#6c71c4")
        (yae-ansi-brightred . "#dc322f")
        (yae-ansi-brightwhite . "#002b36")
        (yae-ansi-brightyellow . "#b58900")
        (yae-ansi-cyan . "#2aa198")
        (yae-ansi-green . "#859900")
        (yae-ansi-magenta . "#6c71c4")
        (yae-ansi-red . "#dc322f")
        (yae-ansi-white . "#586e75")
        (yae-ansi-yellow . "#b58900")))
    )
  "List of color palettes for themes."
  :type '(alist :key-type symbol :value-type (repeat color))
  :group 'yae-simple-theme)

(defcustom yae-selected-palette 'palenight
  "The currently selected color palette."
  :type 'symbol
  :group 'yae-simple-theme)

(deftheme yae_simple
  "Adopted simple form of base16-themes for personal use"
)

(let* ((color-cells (display-color-cells)) ; Get the number of available colors
       ;; Retrieve the colors associated with the selected palette
       (colors (cdr (assoc yae-selected-palette yae-color-palettes)))

       (yae_simple/base00 (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-black colors))
                            "black"))
       (yae_simple/base01 (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-brightgreen colors)) 
                            "brightgreen"))
       (yae_simple/base02 (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-brightyellow colors)) 
                            "brightyellow"))
       (yae_simple/base03 (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-brightblack colors)) 
                            "brightblack"))
       (yae_simple/base04 (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-brightblue colors)) 
                            "brightblue"))
       (yae_simple/base05 (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-white colors)) 
                            "white"))
       (yae_simple/base06 (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-brightmagenta colors)) 
                            "brightmagenta"))
       (yae_simple/base07 (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-brightwhite colors)) 
                            "brightwhite"))
       (yae_simple/base08 (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-red colors)) 
                            "red"))
       (yae_simple/base09 (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-brightred colors)) 
                            "brightred"))
       (yae_simple/base0A (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-yellow colors)) 
                            "yellow"))
       (yae_simple/base0B (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-green colors)) 
                            "green"))
       (yae_simple/base0C (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-cyan colors)) 
                            "cyan"))
       (yae_simple/base0D (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-blue colors)) 
                            "blue"))
       (yae_simple/base0E (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-magenta colors)) 
                            "magenta"))
       (yae_simple/base0E (if (and colors (> color-cells 256)) 
                              (cdr (assoc 'yae-ansi-brightcyan colors)) 
                            "brightcyan"))
       )

  (custom-theme-set-faces
   'yae_simple
   `(bold                         ((t (:bold t))))
   `(bold-italic                  ((t (:bold t))))
   `(border-glyph                 ((t (nil))))
   `(buffers-tab                  ((t (:foreground ,yae_simple/base05 :background ,yae_simple/base00))))
   ;; `(mode-line                    ((t (:foreground ,yae_simple/base07 :background ,yae_simple/base01 :box nil))))
   ;; `(mode-line-highlight          ((t (:foreground ,yae_simple/base08 :box nil))))
   `(hl-line                      ((t (:background ,yae_simple/base02))))     ;; Highlight line background 
   ;; `(region                       ((t (:background ,yae_simple/base02))))     ;; Selection region color 
   ;; `(italic                       ((t (nil))))
   `(left-margin                  ((t (nil))))
   `(toolbar                      ((t (nil))))
   ;; `(underline                    ((nil (:underline nil))))))

;;;; basic colors
     `(border                                       ((t (:background ,yae_simple/base03))))
     `(cursor                                       ((t (:background ,yae_simple/base08))))
     `(default                                      ((t (:foreground ,yae_simple/base05 :background ,yae_simple/base00))))
     ;; `(fringe                                       ((t (:background ,yae_simple/base16-settings-fringe-bg))))
     `(fringe                       ((t (:background ,yae_simple/base00))))
     `(gui-element                                  ((t (:background ,yae_simple/base01))))
     `(header-line                                  ((t (:foreground ,yae_simple/base0E :inherit mode-line))))
     `(highlight                                    ((t (:background ,yae_simple/base00))))
     `(link                                         ((t (:foreground ,yae_simple/base0D :underline t))))
     `(link-visited                                 ((t (:foreground ,yae_simple/base0E :underline t))))
     `(minibuffer-prompt                            ((t (:foreground ,yae_simple/base0D))))
     `(region                                       ((t (:background ,yae_simple/base03 :distant-foreground ,yae_simple/base05))))
     `(secondary-selection                          ((t (:inherit 'default :distant-foreground ,yae_simple/base05 :inverse-video t))))
     ;; `(trailing-whitespace                          ((t (:foreground ,yae_simple/base0A :background ,yae_simple/base0C))))
     `(vertical-border                              ((t (:foreground ,yae_simple/base02))))
     `(widget-button                                ((t (:underline t))))
     `(widget-field                                 ((t (:background ,yae_simple/base03 :box (:line-width 1 :color ,yae_simple/base06)))))

     `(error                                        ((t (:foreground ,yae_simple/base08 :weight bold))))
     `(warning                                      ((t (:foreground ,yae_simple/base09 :weight bold))))
     `(success                                      ((t (:foreground ,yae_simple/base0B :weight bold))))
     `(shadow                                       ((t (:foreground ,yae_simple/base03))))
   
;;;; font-lock
     `(font-lock-builtin-face                       ((t (:foreground ,yae_simple/base0C))))
     `(font-lock-comment-delimiter-face             ((t (:foreground ,yae_simple/base03))))
     `(font-lock-comment-face                       ((t (:foreground ,yae_simple/base03 :italic t))))
     `(font-lock-constant-face                      ((t (:foreground ,yae_simple/base09))))
     `(font-lock-doc-face                           ((t (:foreground ,yae_simple/base04))))
     `(font-lock-doc-string-face                    ((t (:foreground ,yae_simple/base03))))
     `(font-lock-function-name-face                 ((t (:foreground ,yae_simple/base0D))))
     `(font-lock-keyword-face                       ((t (:foreground ,yae_simple/base0E))))
     `(font-lock-negation-char-face                 ((t (:foreground ,yae_simple/base0B))))
     `(font-lock-preprocessor-face                  ((t (:foreground ,yae_simple/base0D))))
     `(font-lock-regexp-grouping-backslash          ((t (:foreground ,yae_simple/base0A))))
     `(font-lock-regexp-grouping-construct          ((t (:foreground ,yae_simple/base0E))))
     `(font-lock-string-face                        ((t (:foreground ,yae_simple/base0B))))
     `(font-lock-type-face                          ((t (:foreground ,yae_simple/base0A))))
     `(font-lock-variable-name-face                 ((t (:foreground ,yae_simple/base08))))
     `(font-lock-warning-face                       ((t (:foreground ,yae_simple/base08))))

;;;; ansi-colors)
     `(ansi-color-black                             ((t (:foreground ,yae_simple/base02 :background ,yae_simple/base00))))
     `(ansi-color-white                             ((t (:foreground ,yae_simple/base05 :background ,yae_simple/base07))))
     `(ansi-color-red                               ((t (:foreground ,yae_simple/base08 :background ,yae_simple/base08))))
     `(ansi-color-yellow                            ((t (:foreground ,yae_simple/base0A :background ,yae_simple/base0A))))
     `(ansi-color-green                             ((t (:foreground ,yae_simple/base0B :background ,yae_simple/base0B))))
     `(ansi-color-cyan                              ((t (:foreground ,yae_simple/base0C :background ,yae_simple/base0C))))
     `(ansi-color-blue                              ((t (:foreground ,yae_simple/base0D :background ,yae_simple/base0D))))
     `(ansi-color-magenta                           ((t (:foreground ,yae_simple/base0E :background ,yae_simple/base0E))))


;; ;;;; term and ansi-term)
;;      `(term                                         ((t (:foreground ,yae_simple/base05 :background ,yae_simple/base00))))
;;      `(term-color-black                             ((t (:foreground ,yae_simple/base02 :background ,yae_simple/base00))))
;;      `(term-color-white                             ((t (:foreground ,yae_simple/base05 :background ,yae_simple/base07))))
;;      `(term-color-red                               ((t (:foreground ,yae_simple/base08 :background ,yae_simple/base08))))
;;      `(term-color-yellow                            ((t (:foreground ,yae_simple/base0A :background ,yae_simple/base0A))))
;;      `(term-color-green                             ((t (:foreground ,yae_simple/base0B :background ,yae_simple/base0B))))
;;      `(term-color-cyan                              ((t (:foreground ,yae_simple/base0C :background ,yae_simple/base0C))))
;;      `(term-color-blue                              ((t (:foreground ,yae_simple/base0D :background ,yae_simple/base0D))))
;;      `(term-color-magenta                           ((t (:foreground ,yae_simple/base0E :background ,yae_simple/base0E))))

     ;; `(header-line                           ((t (:inherit default :background ,yae_simple/base00))))

     `(mode-line                             ((t (:foreground ,yae_simple/base07 :background ,yae_simple/base03 :box t))))

   ;; ;;; modeline
     `(mode-line-inactive ((t (:inherit variable-pitch :foreground ,yae_simple/base05 :background ,yae_simple/base00 :box t))))
   ;;   `(mode-line-buffer-id ((t (:inherit mode-line))))

	 ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun yae_simple-theme()
  "Load yae_simple-theme."
  (interactive)
  (load-theme 'yae_simple t))

(provide-theme 'yae_simple)

;;; yae_simple-theme.el ends here
