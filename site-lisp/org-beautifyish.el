;;; org-beautify.el --- Beautify Org mode headers based on #+beautified keyword -*- lexical-binding: t; -*-


;; Author: Yilkal Argaw
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: org, beautify
;; URL: https://github.com/yilkalargaw/rtfm-emacs/site-lisp/org-beautifyish.el

;;; Commentary:

;; This package applies font face remapping in Org mode buffers according to
;; the #+beautified keyword at the top of the buffer.

;;; Code:
(defvar-local org-beautifyish--remap-cookies nil
  "List of face remap cookies for undoing remaps in this buffer.")

(defun org-beautifyish-clear-remaps ()
  "Clear face remaps made by `org-beautifyish-buffer`."
  (when org-beautifyish--remap-cookies
    (mapc #'face-remap-remove-relative org-beautifyish--remap-cookies)
    (setq org-beautifyish--remap-cookies nil)))

;;;###autoload
(defun org-beautifyish-buffer ()
  "Apply beautified font styles in Org mode based on the #+beautified keyword."
  (when (derived-mode-p 'org-mode)
    (org-beautifyish-clear-remaps)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (if (re-search-forward "^#\\+beautified: *\\(.*\\)$" nil t)
            (let ((value (downcase (string-trim (match-string 1)))))
              (cond
               ((member value '("1 1" "1" "t"))
                (let ((sizes '(1.2 1.15 1.1 1.05 1.0 1.0 1.0)))
                  (dotimes (i (length sizes))
                    (let* ((level (1+ i))
                           (face (intern (format "org-level-%d" level)))
                           (scale (nth i sizes)))
                      (push (face-remap-add-relative face
                                                     `(:family "Sans" :height ,scale :weight normal))
                            org-beautifyish--remap-cookies)))))

               ((member value '("1 2"))
                (let ((sizes '(1.2 1.15 1.1 1.05 1.0 1.0 1.0)))
                  (dotimes (i (length sizes))
                    (let* ((level (1+ i))
                           (face (intern (format "org-level-%d" level)))
                           (scale (nth i sizes))
                           (weight (if (<= level 5) 'bold 'normal)))
                      (push (face-remap-add-relative face
                                                     `(:family "Sans" :height ,scale :weight ,weight))
                            org-beautifyish--remap-cookies)))))

               ((member value '("2 1" "2"))
                (push (face-remap-add-relative 'default '(:family "Sans" :height 1.05))
                      org-beautifyish--remap-cookies)
                (dolist (face '(org-code org-block org-block-begin-line
                                         org-block-end-line org-table org-verbatim org-formula))
                  (push (face-remap-add-relative face '(:inherit fixed-pitch :family "Monospace"))
                        org-beautifyish--remap-cookies))
                (let ((sizes '(1.2 1.15 1.1 1.05 1.0 1.0 1.0)))
                  (dotimes (i (length sizes))
                    (let* ((level (1+ i))
                           (face (intern (format "org-level-%d" level)))
                           (scale (nth i sizes)))
                      (push (face-remap-add-relative face
                                                     `(:family "Sans" :height ,scale :weight normal))
                            org-beautifyish--remap-cookies)))))

               ((string= value "2 2")
                (push (face-remap-add-relative 'default '(:family "Sans" :height 1.05))
                      org-beautifyish--remap-cookies)
                (dolist (face '(org-code org-block org-block-begin-line
                                         org-block-end-line org-table org-verbatim org-formula))
                  (push (face-remap-add-relative face '(:inherit fixed-pitch :family "Monospace"))
                        org-beautifyish--remap-cookies))
                (let ((sizes '(1.2 1.15 1.1 1.05 1.0 1.0 1.0)))
                  (dotimes (i (length sizes))
                    (let* ((level (1+ i))
                           (face (intern (format "org-level-%d" level)))
                           (scale (nth i sizes))
                           (weight (if (<= level 5) 'bold 'normal)))
                      (push (face-remap-add-relative face
                                                     `(:family "Sans" :height ,scale :weight ,weight))
                            org-beautifyish--remap-cookies)))))

               (t (buffer-face-mode -1))))
          ;; No #+beautified line: reset
          (buffer-face-mode -1))))))
;;;###autoload
(define-minor-mode org-beautifyish-mode
  "Minor mode to beautify Org mode headers based on #+beautified keyword."
  :lighter " OrgBeautify"
  (if org-beautifyish-mode
      (progn
        (org-beautifyish-buffer)
        (add-hook 'after-save-hook #'org-beautifyish-buffer nil t))
    (org-beautifyish-clear-remaps)
    (remove-hook 'after-save-hook #'org-beautifyish-buffer t)))

(provide 'org-beautifyish)

;;; org-beautifyish.el ends here
