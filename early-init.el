;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

(require 'org)
(org-babel-tangle-file (expand-file-name "early-init.org" user-emacs-directory))

;;; early-init.el ends here
