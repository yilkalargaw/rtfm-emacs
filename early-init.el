;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; Minimal early-init.el - JUST for bootstrapping
(setq package-enable-at-startup nil)  ; Disable package.el until init

;; Compile config.org if needed
(let ((org-file (expand-file-name "early-init.org" user-emacs-directory)))
  (when (or (not (file-exists-p (expand-file-name "early-init.elc" user-emacs-directory)))
            (file-newer-than-file-p org-file
                                   (expand-file-name "early-init.elc" user-emacs-directory)))
    (require 'org)
    (require 'ox)
    (org-babel-tangle-file org-file
                          (expand-file-name "early-init.el" user-emacs-directory))
    (byte-compile-file (expand-file-name "early-init.el" user-emacs-directory))))

;; Load the compiled version
(load (expand-file-name "early-init.elc" user-emacs-directory) t t)

;;; early-init.el ends here
