;;; markdown-dumb-mode.el --- A simple, dumb Markdown major mode -*- lexical-binding: t; -*-

;; Author: Yilkal Argaw
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: markdown, major-mode, editing
;; URL: https://github.com/yilkalargaw/rtfm-emacs/site-lisp/markdown-dumb.el

;;; Commentary:

;; markdown-dumb-mode is a very lightweight and minimal Emacs major mode
;; for editing Markdown files without any dependencies.
;;
;; Features:
;; - Syntax highlighting for headers, bold, italic, and inline code
;; - Highlighting for fenced code blocks (```), but no language parsing
;; - YAML front matter highlighting (delimited by ---)
;; - Basic comment support using <!-- ... -->
;; - Compatible with Emacs' comment-dwim and comment-region commands
;;
;; This mode is intentionally "dumb": it does not parse Markdown fully,
;; does not preview, and does not rely on external tools.
;;
;; To use it, put this file in your load path and add the following to
;; your Emacs configuration:
;;
;;   (require 'markdown-dumb-mode)
;;
;; Files ending in .md will automatically open in markdown-dumb-mode.

;;; Code:

(defvar markdown-dumb-mode-hook nil)

(defvar markdown-dumb-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Treat # as comment starter (YAML-style)
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Treat ` as string delimiter
    (modify-syntax-entry ?` "\"" st)
    st)
  "Syntax table for `markdown-dumb-mode'.")

(defface markdown-dumb-code-block-face
  '((t (:inherit font-lock-constant-face :background "#f0f0f0")))
  "Face for fenced code blocks.")

(defface markdown-dumb-yaml-face
  '((t (:inherit font-lock-comment-face :slant italic)))
  "Face for YAML front matter.")

(defconst markdown-dumb-font-lock-keywords
  `(
    ;; YAML front matter
    ("\\`---\\(\n\\|.\\)*?\n---" . 'markdown-dumb-yaml-face)

    ;; Headings
    ("^#\\{1\\} .*" . font-lock-function-name-face)
    ("^#\\{2\\} .*" . font-lock-variable-name-face)
    ("^#\\{3\\} .*" . font-lock-keyword-face)

    ;; Bold and Italic
    ("\\*\\*\\([^*\n]+\\)\\*\\*" . (1 font-lock-bold-face))
    ("\\*\\([^*\n]+\\)\\*" . (1 font-lock-italic-face))

    ;; Inline code
    ("`\\([^`\n]+\\)`" . (1 font-lock-constant-face))

    ;; Code blocks
    ("^```[a-zA-Z0-9]*\\(\\(.\\|\n\\)*?\\)^```$" . 'markdown-dumb-code-block-face)

    ;; HTML comments
    ("<!--\\(.\\|\n\\)*?-->" . font-lock-comment-face)
    ))

(defun markdown-dumb-comment-dwim (arg)
  "Comment or uncomment current line or region in a dumb Markdown way.
Uses <!-- --> for block comments."
  (interactive "*P")
  (let ((comment-start "<!-- ")
        (comment-end " -->"))
    (comment-dwim arg)))

;;;###autoload
(define-derived-mode markdown-dumb-mode fundamental-mode "Markdown-Dumb"
  "A dumb minimal mode for Markdown files."
  :syntax-table markdown-dumb-mode-syntax-table

  (setq font-lock-defaults '((markdown-dumb-font-lock-keywords) t))

  ;; Set comment markers for comment-region, etc.
  (setq-local comment-start "<!-- ")
  (setq-local comment-end " -->")
  (setq-local comment-start-skip "<!--+\\s-*")
  (setq-local comment-end-skip "\\s-*-->+")
  (setq-local comment-use-syntax nil)

  ;; Set the comment command
  (setq-local comment-line-function #'markdown-dumb-comment-dwim)

  ;;set whitespace settings
  (setq-local whitespace-style
             '(face                 ;; enable highlighting
               trailing             ;; highlight trailing spaces
               tabs                 ;; highlight tabs
               spaces               ;; highlight spaces
               lines-tail           ;; highlight long lines
               indentation))        ;; highlight mixed indentation
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-dumb-mode))

(provide 'markdown-dumb-mode)

;;; markdown-dumb-mode.el ends here
