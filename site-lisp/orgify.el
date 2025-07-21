;;; orgify.el --- Convert buffers or regions to Org mode -*- lexical-binding: t; -*-

;; Author: Yilkal Argaw
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, tools, org, markdown
;; URL: https://github.com/yilkalargaw/rtfm-emacs/site-lisp/orgify.el

;;; Commentary:

;; orgify provides a command to convert the current buffer or active region
;; into an Org mode formatted buffer.
;;
;; Supported input formats include programming modes (wrapped in source blocks),
;; Markdown, LaTeX, HTML, MHTML, Text, Man pages, Woman pages, Troff/Groff,
;; SGML
;;
;; Special handling for `eww-mode` buffers uses Emacs built-in function
;; `org-eww-copy-for-org-mode` to convert the rendered web content into Org
;; with links properly preserved.
;;
;; For unsupported modes such as `dired-mode` (configurable via
;; `orgify-unsupported-modes`), the command refuses to run and warns the user.
;;
;; The command supports converting only the active region if any, otherwise
;; the whole buffer is converted.
;;
;; Usage:
;; M-x orgify
;; Converts the current buffer or region into an Org formatted scratch buffer
;; named *orgify*.

;;; Code:

(require 'org)
(require 'ol nil t)
(require 'ol-eww nil t)
(require 'subr-x) ;; for `string-trim`

(defvar orgify-unsupported-modes
  '(dired-mode
    ansi-term-mode
    term-mode
    eshell-mode)
  "List of major modes in which `orgify` should not be used.")

(defun orgify--asciidoc-to-org (text)
  "Convert basic AsciiDoc TEXT to Org-mode markup."
  (let ((converted text))
    (setq converted
          (replace-regexp-in-string "^\\(=+\\)[ \t]+"
                                    (lambda (_)
                                      (concat (make-string (length (match-string 1 converted)) ?*) " "))
                                    converted))
    (setq converted
          (replace-regexp-in-string "\\(^\\|[^_]\\)_\\([^_]+\\)_\\([^_]\\|$\\)"
                                    "\\1/\\2/\\3" converted))
    (setq converted
          (replace-regexp-in-string "^\\([*-]\\)[ \t]+" "- " converted))
    (setq converted
          (replace-regexp-in-string "^\\[quote\\][ \t\n]*----[ \t\n]*"
                                    "#+BEGIN_QUOTE\n" converted))
    (setq converted
          (replace-regexp-in-string "^----[ \t\n]*" "#+END_QUOTE\n" converted))
    (setq converted
          (replace-regexp-in-string "^\\[source,\\([^]]+\\)\\][ \t\n]*----[ \t\n]*"
                                    "#+BEGIN_SRC \\1\n" converted))
    (setq converted
          (replace-regexp-in-string "^----[ \t\n]*" "#+END_SRC\n" converted))
    (setq converted
          (replace-regexp-in-string "^:[^:]+:.*$" "" converted))
    (setq converted
          (replace-regexp-in-string "`\\([^`]+\\)`" "=\1=" converted))
    converted))

(defun orgify ()
  "Convert current buffer or region to Org mode format in a new *orgify* buffer."
  (interactive)
  (let ((mode-name (symbol-name major-mode)))
    (cond
     ;; EWW-mode
     ((and (eq major-mode 'eww-mode)
           (fboundp 'org-eww-copy-for-org-mode))
      (org-eww-copy-for-org-mode)
      (let ((converted-text (current-kill 0 t)))
        (with-current-buffer (generate-new-buffer "*orgify*")
          (insert converted-text)
          (org-mode)
          (switch-to-buffer (current-buffer))
          (goto-char (point-min)))))

     ;; Unsupported modes
     ((member major-mode orgify-unsupported-modes)
      (user-error "Orgify is not supported in %s" mode-name))

     (t
      ;; Get text from region or buffer
      (let* ((region-active (use-region-p))
             (text (buffer-substring-no-properties
                    (if region-active (region-beginning) (point-min))
                    (if region-active (region-end) (point-max))))
             (converted-text
              (cond
               ;; Programming modes → Org source block
               ((derived-mode-p 'prog-mode)
                (let* ((lang-mode (or (car (rassoc major-mode org-src-lang-modes))
                                      (replace-regexp-in-string "-mode\\'" "" mode-name))))
                  (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n" lang-mode text)))

               ;; AsciiDoc (native conversion)
               ((memq major-mode '(adoc-mode asciidoc-dumb-mode))
                (orgify--asciidoc-to-org text))

               ;; Other modes using Pandoc (if available)
               ((executable-find "pandoc")
                (let* ((mode-to-format
                        '(("html-mode"             . "html")
                          ("mhtml-mode"            . "html")
                          ("markdown-mode"         . "markdown")
                          ("markdown-dumb-mode"    . "markdown")
                          ("latex-mode"            . "latex")
                          ("text-mode"             . "plain")
                          ("man-mode"              . "man")
                          ("woman-mode"            . "man")
                          ("nroff-mode"            . "roff")
                          ("sgml-mode"             . "html")
                          ("sxml-mode"             . "html")
                          ("fundamental-mode"      . "plain")
                          ("Fundamental"           . "plain")))
                       (pandoc-from (assoc-default mode-name mode-to-format #'string=)))
                  (unless pandoc-from
                    (error "No known Pandoc format mapping for mode: %s" mode-name))
                  (with-temp-buffer
                    (insert text)
                    (let ((exit-code
                           (call-process-region
                            (point-min) (point-max)
                            "pandoc" t t nil
                            "-f" pandoc-from
                            "-t" "org")))
                      (if (zerop exit-code)
                          (buffer-string)
                        (error "Pandoc conversion failed for %s" mode-name))))))

               ;; Fallback
               (t
                (user-error "Don't know how to orgify buffer in mode: %s" mode-name)))))

        ;; Show result in new buffer
        (with-current-buffer (generate-new-buffer "*orgify*")
          (insert converted-text)
          (org-mode)
          (switch-to-buffer (current-buffer))
          (goto-char (point-min))))))))

(provide 'orgify)
