;;; asciidoc-dumb-mode.el --- A simple, dumb AsciiDoc major mode -*- lexical-binding: t; -*-

;; Author: Yilkal Argaw
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: asciidoc, major-mode, editing
;; URL: https://github.com/yilkalargaw/rtfm-emacs/site-lisp/ascii-dumb-mode.el

;;; Commentary:

;; asciidoc-dumb-mode is a minimal major mode for editing AsciiDoc files
;; without dependencies.
;;
;; Features:
;; - Syntax highlighting for headings, bold, italic, and inline code
;; - Highlighting for source blocks ([source] + fenced with ----)
;; - Basic comment support using `//`
;; - Compatible with Emacs comment-dwim and comment-region commands
;;
;; This mode is intentionally simple and dumb:
;; no parsing, no preview, just basic editing support.
;;
;; To use, add this file to your load path and add to your Emacs config:
;;
;;   (require 'asciidoc-dumb-mode)
;;
;; Files ending with .adoc will auto-activate this mode.

;;; Code:

(defvar asciidoc-dumb-mode-hook nil)

(defvar asciidoc-dumb-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Treat / as comment start for line comments //
    (modify-syntax-entry ?/ "< 12" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Treat ` as string delimiter (for inline code)
    (modify-syntax-entry ?` "\"" st)
    st)
  "Syntax table for `asciidoc-dumb-mode'.")

(defface asciidoc-dumb-source-block-face
  '((t (:inherit font-lock-constant-face :background "#f0f0f0")))
  "Face for source code blocks.")

(defconst asciidoc-dumb-font-lock-keywords
  `(
    ;; Headings: = Level 1, == Level 2, === Level 3, etc.
    ("^=+\\s-+.*$" . font-lock-function-name-face)

    ;; Bold: *bold*
    ("\\*\\([^*\n]+\\)\\*" . (1 font-lock-bold-face))

    ;; Italic: _italic_
    ("_\\([^_\n]+\\)_" . (1 font-lock-italic-face))

    ;; Inline code: `code`
    ("`\\([^`\n]+\\)`" . (1 font-lock-constant-face))

    ;; Source blocks: [source] followed by ---- fenced block
    ("^\\[source\\]\\s-*\n----\\(\\(.\\|\n\\)*?\\)^----$" . 'asciidoc-dumb-source-block-face)

    ;; Line comments: lines starting with //
    ("^//.*$" . font-lock-comment-face)
    ))

(defun asciidoc-dumb-comment-dwim (arg)
  "Comment or uncomment current line or region using `//` comments."
  (interactive "*P")
  (let ((comment-start "// "))
    (comment-dwim arg)))

;;;###autoload
(define-derived-mode asciidoc-dumb-mode fundamental-mode "AsciiDoc-Dumb"
  "A dumb minimal major mode for AsciiDoc files."
  :syntax-table asciidoc-dumb-mode-syntax-table

  (setq font-lock-defaults '((asciidoc-dumb-font-lock-keywords) t))

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")
  (setq-local comment-end-skip nil)
  (setq-local comment-use-syntax nil)

  (setq-local comment-line-function #'asciidoc-dumb-comment-dwim))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . asciidoc-dumb-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . asciidoc-dumb-mode))
(add-to-list 'auto-mode-alist '("\\.asc\\'" . asciidoc-dumb-mode))

(provide 'asciidoc-dumb-mode)

;;; asciidoc-dumb-mode.el ends here
