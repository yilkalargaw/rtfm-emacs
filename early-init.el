;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold (if (display-graphic-p) (* 400 1024 1024 ) (* 128 1024 1024)))

;; ;; Defer garbage collection further back in the startup process
;; (setq gc-cons-threshold most-positive-fixnum)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize nil)

(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
(push '(left-fringe    . 8) default-frame-alist)
(push '(right-fringe   . 8) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

;; Vertical window divider
(when (not (version<= emacs-version "25.0"))
  (setq window-divider-default-right-width 4
        window-divider-default-places 'right-only)
  (window-divider-mode 1))

;; ;; No ugly button for checkboxes
;; (setq widget-image-enable nil)


;; Only disable GUI elements if running in a graphical display
(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (display-graphic-p frame)
              (with-selected-frame frame
                ;; (set-frame-size frame 93 50)
                ;; (set-frame-position frame 0 15)
                ;; (set-window-margins nil 2)
                (scroll-bar-mode -1)
                (menu-bar-mode -1)
                (tool-bar-mode -1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some minor ui and font settings
(set-window-margins nil 2)
(setq inhibit-startup-message t)
;; (global-set-key (kbd "C-c m b") 'menu-bar-mode)
(column-number-mode 1)
(setq initial-scratch-message nil)
(setq battery-mode-line-format " [%b%p%% } ")
(display-battery-mode t)
;; (display-time-mode t)
(setq frame-resize-pixelwise t)
(setq redisplay-skip-fontification-on-input t)
(setq idle-update-delay 1.0)
(setq compilation-scroll-output t)

;; (set-face-attribute 'default nil :font "Droid Sans Mono")
;; (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend)

(defun yae-set-font (&optional frame)
  (when (display-graphic-p (or frame (selected-frame)))
    (with-selected-frame (or frame (selected-frame))
      (set-frame-font "JetBrainsMono Nerd Font")
      ;; (set-frame-font "LiterationMono Nerd Font")
      ;; (set-frame-font "CommitMono Nerd Font")
      ;; (set-frame-font "MonaspiceNe Nerd Font")
      (set-face-attribute 'default nil :height 95))))

(when (display-graphic-p)
  (yae-set-font))

(add-hook 'after-make-frame-functions 'yae-set-font)

;; Fontset settings – only safe to apply in GUI
(when (display-graphic-p)
  ;; (setq default-frame-alist '((font . "JetBrainsMono Nerd Font 9")))
  ;; (add-to-list 'default-frame-alist (cons 'font  my-default-font))
  (set-fontset-font t nil "Dejavu Sans Mono" nil 'append)
  (set-fontset-font t 'ethiopic "Noto Sans Ethiopic")
  ;; (set-fontset-font t 'thai "Leelawadee UI" nil 'append)
  ;; (setq w32-enable-synthesized-fonts t) ;;The w32-enable-synthesized-fonts variable is obsolete starting from Emacs 24.4, as Emacs no longer has this limitation.
  ;; (set-face-font 'italic "-*-Courier New-normal-i-*-*-11-*-*-*-c-*-iso8859-1")
  ;; (set-face-font 'bold-italic "-*-Courier New-bold-i-*-*-11-*-*-*-c-*-iso8859-1")
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;; increase garbage collection threshold for startup only
;; ;; (setq gc-cons-threshold 8000000)
;; (setq gc-cons-threshold (if (display-graphic-p) (* 400 1024 1024 ) (* 128 1024 1024)))
;; (setq gc-cons-percentage 0.7))

(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               ;; (setq gc-cons-threshold 800000) ;; default
                               (setq gc-cons-threshold (* 32 1024 1024))
                               (setq gc-cons-percentage 0.5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
