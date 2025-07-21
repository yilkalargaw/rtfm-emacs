;;; presentation-mode.el --- Simple Org presentation using level-1 headings -*- lexical-binding: t; -*-

;; Author: Yilkal Argaw
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.0"))
;; Keywords: org, presentation, convenience
;; URL: https://github.com/yilkalargaw/rtfm-emacs/site-lisp/presentation-mode.el
;;; Commentary:

;; Minimal presentation mode for Org buffers using level-1 headings as slides.
;; Supports slide navigation, read-only buffer, inline images, and header updates.

;;; Code:

(require 'org)

(defvar presentation--slide-headings nil
  "List of positions of all level-1 headings in the current buffer.")

(defvar presentation--current-slide-index 0
  "Index of the current slide in `presentation--slide-headings`.")

(defvar presentation--original-settings nil
  "Alist of original buffer-local settings to restore when quitting presentation.")

(defvar-local presentation--inline-images-shown nil
  "Whether inline images were shown before presentation started.")

(defun presentation--collect-level-1-headings ()
  "Collect positions of all level-1 Org headings in the current buffer."
  (setq presentation--slide-headings nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (when (= (org-current-level) 1)
        (push (point) presentation--slide-headings)))
    (setq presentation--slide-headings (reverse presentation--slide-headings))))

(defun presentation--narrow-to-subtree-and-center ()
  "Narrow to current subtree with heading at top and vertically center content."
  (widen)
  (org-show-all)
  (org-narrow-to-subtree)
  (goto-char (point-min))
  (recenter 0)

  ;; Remove old padding overlays
  (remove-overlays (point-min) (point-max) 'presentation-padding t)

  ;; Calculate padding to vertically center content
  (let* ((content-lines (count-lines (point-min) (point-max)))
         (window-lines (window-body-height))
         (blank-lines-needed (max 0 (/ (- window-lines content-lines) 2))))
    (when (> blank-lines-needed 0)
      (let ((ov (make-overlay (point-max) (point-max))))
        (overlay-put ov 'presentation-padding t)
        (overlay-put ov 'after-string (propertize "\n" 'line-height blank-lines-needed))))))

(defun presentation--goto-slide (index)
  "Display slide at INDEX."
  (when (and presentation--slide-headings
             (>= index 0)
             (< index (length presentation--slide-headings)))
    (setq presentation--current-slide-index index)
    (widen)
    (goto-char (nth index presentation--slide-headings))
    (org-back-to-heading)
    (presentation--narrow-to-subtree-and-center)
    (presentation--update-header)))

(defun presentation--update-header ()
  "Update header line with slide number."
  (setq header-line-format
        (format " Slide %d of %d "
                (1+ presentation--current-slide-index)
                (length presentation--slide-headings))))

(defun presentation--inline-images-shown-p ()
  "Check if inline image overlays are present in the buffer."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (not (eobp))
        (let ((ovs (overlays-at (point)))
              (found nil))
          (while (and ovs (not found))
            (if (eq (overlay-get (car ovs) 'org-image-overlay) t)
                (throw 'found t)
              (setq ovs (cdr ovs))))
          (forward-char 1)))
      nil)))

;;;###autoload
(defun presentation-start ()
  "Start presentation mode for current Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "Presentation mode only works in Org mode buffers"))

  ;; Save original settings to restore later
  (setq presentation--original-settings
        `((truncate-lines . ,truncate-lines)
          (scroll-margin . ,scroll-margin)
          (mode-line-format . ,mode-line-format)
          (header-line-format . ,header-line-format)
          (display-line-numbers . ,display-line-numbers)))

  ;; Set buffer read-only
  (setq-local buffer-read-only t)

  ;; Save whether inline images are currently shown
  (setq presentation--inline-images-shown (presentation--inline-images-shown-p))

  ;; Display inline images forcibly
  (org-display-inline-images nil t)

  ;; Adjust display settings
  (setq-local truncate-lines nil)
  (setq-local scroll-margin 0)
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (setq-local display-line-numbers nil)

  ;; Collect slides and show first
  (presentation--collect-level-1-headings)
  (if presentation--slide-headings
      (progn
        (setq presentation--current-slide-index 0)
        (presentation--goto-slide 0)

        ;; Bind keys locally
        (local-set-key (kbd "<right>") #'presentation-next-slide)
        (local-set-key (kbd "<left>")  #'presentation-prev-slide)
        (local-set-key (kbd "n")       #'presentation-next-slide)
        (local-set-key (kbd "p")       #'presentation-prev-slide)
        (local-set-key (kbd "q")       #'presentation-quit)
        (local-set-key (kbd "<escape>") #'presentation-quit)
        (local-set-key (kbd "S-<f5>") #'presentation-quit)

        (message "Presentation started. Use n/p or arrows to navigate. q or ESC to quit."))
    (message "No level-1 headings found; cannot start presentation.")))

;;;###autoload
(defun presentation-next-slide ()
  "Go to next slide."
  (interactive)
  (let ((next (1+ presentation--current-slide-index)))
    (if (>= next (length presentation--slide-headings))
        (message "End of presentation")
      (presentation--goto-slide next))))

;;;###autoload
(defun presentation-prev-slide ()
  "Go to previous slide."
  (interactive)
  (let ((prev (1- presentation--current-slide-index)))
    (if (< prev 0)
        (message "Beginning of presentation")
      (presentation--goto-slide prev))))

;;;###autoload
(defun presentation-quit ()
  "Quit presentation mode and restore buffer settings."
  (interactive)
  (widen)

  ;; Restore inline images to previous state
  (if presentation--inline-images-shown
      (org-display-inline-images nil t)
    (org-remove-inline-images))

  ;; Restore buffer-local settings
  (dolist (setting presentation--original-settings)
    (set (make-local-variable (car setting)) (cdr setting)))

  ;; Unbind local keys
  (local-unset-key (kbd "<right>"))
  (local-unset-key (kbd "<left>"))
  (local-unset-key (kbd "n"))
  (local-unset-key (kbd "p"))
  (local-unset-key (kbd "q"))
  (local-unset-key (kbd "<escape>"))
  (local-unset-key (kbd "S-<f5>"))

  ;; Allow editing again
  (setq-local buffer-read-only nil)

  (setq header-line-format nil)
  (setq presentation--slide-headings nil)
  (message "Presentation ended."))

(provide 'presentation-mode)

;;; presentation-mode.el ends here
