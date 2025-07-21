;;; org-tocish.el --- Generate and update Org TOC blocks -*- lexical-binding: t; -*-

;; Author: Your Name <your.email@example.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (org "9.0"))
;; Keywords: org, convenience, toc
;; URL: https://github.com/yilkalargaw/rtfm-emacs/site-lisp/org-tocish.el

;;; Commentary:

;; This package provides functions to generate and replace table of contents
;; blocks in Org buffers, respecting TOC depth options and a minor mode to
;; automatically update TOC before saving.

;;; Code:

(require 'org)

(defun org-tocish--generate-toc (toc-depth)
  "Generate a table of contents for the current Org buffer respecting TOC-DEPTH."
  (let ((toc ""))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (let ((level (org-element-property :level headline))
              (title (org-element-property :raw-value headline)))
          (when (and (<= level toc-depth)
                     (not (org-element-property :commentedp headline)))
            (setq toc (concat toc (make-string (* 2 (- level 1)) ?\ )
                              "- [[" title "]]\n"))))))
    toc))

(defun org-tocish--get-commented-toc ()
  "Return the value of the COMMENTED_TOC property in the current buffer, if any."
  (let ((commented-toc nil))
    (org-map-entries
     (lambda ()
       (let ((toc (org-entry-get nil "COMMENTED_TOC")))
         (when toc
           (setq commented-toc toc)))))
    commented-toc))

;;;###autoload
(defun org-tocish-generate-or-replace-toc ()
  "Generate or replace TOC blocks in the current Org buffer respecting TOC depth."
  (interactive)
  (let ((default-toc-depth 3)
        (commented-toc (org-tocish--get-commented-toc)))
    ;; Determine TOC depth from #+OPTIONS: toc:n or default
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*toc:\\([0-9]+\\)" nil t)
        (setq default-toc-depth (string-to-number (match-string 1)))))
    ;; Replace all TOC blocks
    (save-excursion
      (goto-char (point-min))
      (while (or (re-search-forward "^#\\+BEGIN_TOC\\(?: +:depth +\\([0-9]+\\)\\)? *$" nil t)
                 (re-search-forward "^# #\\+BEGIN_TOC\\(?: +:depth +\\([0-9]+\\)\\)? *$" nil t))
        (let* ((depth (or (and (match-string 1) (string-to-number (match-string 1)))
                          default-toc-depth))
               (start (match-beginning 0))
               (end (progn
                      (or (re-search-forward "^#\\+END_TOC$" nil t)
                          (re-search-forward "^# #\\+END_TOC$" nil t))
                      (match-end 0)))
               (toc (org-tocish--generate-toc depth)))
          (delete-region start end)
          (goto-char start)
          (if commented-toc
              (insert "# #+BEGIN_TOC\n" toc "# #+END_TOC")
            (insert "#+BEGIN_TOC\n" toc "#+END_TOC")))))))

;;;###autoload
(define-minor-mode org-tocish-mode
  "Minor mode to automatically generate/update Org TOC blocks on save."
  :lighter " TOCish"
  (if org-tocish-mode
      (add-hook 'before-save-hook #'org-tocish-generate-or-replace-toc nil t)
    (remove-hook 'before-save-hook #'org-tocish-generate-or-replace-toc t)))

;;;###autoload
(defun org-tocish-enable ()
  "Enable `org-tocish-mode` in Org buffers."
  (when (derived-mode-p 'org-mode)
    (org-tocish-mode 1)))

;;;###autoload
(add-hook 'org-mode-hook #'org-tocish-enable)

(provide 'org-tocish)

;;; org-tocish.el ends here
