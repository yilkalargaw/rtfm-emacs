;;; package --- Summary
;;; Commentary:
;;; Code:

;;; presentation-mode.el --- Level-1 Org headings as slides -*- lexical-binding: t; -*-
;; -*- lexical-binding: t;-*-

(defvar presentation--slide-headings nil
  "List of positions of all level-1 headings.")

(defvar presentation--current-slide-index 0
  "Current index into presentation--slide-headings.")

(defvar presentation--original-settings nil
  "Alist of buffer-local settings to restore after quitting presentation.")

(defun presentation--collect-level-1-headings ()
  "Collect positions of all level-1 Org headings."
  (setq presentation--slide-headings nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (when (= (org-current-level) 1)
        (push (point) presentation--slide-headings)))
    (setq presentation--slide-headings (nreverse presentation--slide-headings))))

(defun presentation--goto-slide (index)
  "Jump to slide at INDEX with proper display."
  (when (and presentation--slide-headings
             (>= index 0)
             (< index (length presentation--slide-headings)))
    (setq presentation--current-slide-index index)
    (widen)
    (goto-char (nth index presentation--slide-headings))
    (org-back-to-heading)  ; Ensure we're at heading start
    (presentation--narrow-to-subtree-and-center)
    (presentation--update-header)))

(defun presentation--narrow-to-subtree-and-center ()
  "Narrow to current subtree with heading at top of screen and all content expanded.
Add invisible padding at the bottom to vertically center content."
  (widen)
  (org-show-all)
  (org-narrow-to-subtree)
  (goto-char (point-min))
  (recenter 0)

  ;; Remove any old padding overlays first
  (remove-overlays (point-min) (point-max) 'presentation-padding t)

  ;; Calculate how many lines to pad
  (let* ((content-lines (count-lines (point-min) (point-max)))
         (window-lines (window-body-height))
         (blank-lines-needed (max 0 (/ (- window-lines content-lines) 2)))) ; half padding top+bottom

    (when (> blank-lines-needed 0)
      ;; Create an overlay at the end with invisible padding
      (let ((ov (make-overlay (point-max) (point-max))))
        (overlay-put ov 'presentation-padding t)
        (overlay-put ov 'after-string
                     (propertize
                      "\n"
                      'line-height blank-lines-needed))))))

(defun presentation--update-header ()
  "Update header to show slide number."
  (setq header-line-format
        (format " Slide %d of %d "
                (1+ presentation--current-slide-index)
                (length presentation--slide-headings))))

(defun presentation-next-slide ()
  "Advance to the next slide."
  (interactive)
  (let ((next (1+ presentation--current-slide-index)))
    (if (>= next (length presentation--slide-headings))
        (message "End of presentation")
      (presentation--goto-slide next))))

(defun presentation-prev-slide ()
  "Go back to the previous slide."
  (interactive)
  (let ((prev (1- presentation--current-slide-index)))
    (if (< prev 0)
        (message "Beginning of presentation")
      (presentation--goto-slide prev))))

(defvar presentation--default-remap-cookie nil
  "Cookie to remove default face remapping when quitting presentation.")

(defvar presentation--heading-remap-cookies nil
  "List of cookies for remapping Org heading faces.")

(defvar-local presentation--face-remap-cookies nil
  "List of face remapping cookies added during presentation mode.")


(defvar-local presentation--inline-images-shown nil
  "Whether inline images were shown before presentation started.")

(defun presentation--inline-images-shown-p ()
  "Return non-nil if inline image overlays are present anywhere in the buffer."
  (catch 'found
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((ovs (overlays-at (point))))
          (when (cl-some (lambda (ov)
                           (eq (overlay-get ov 'org-image-overlay) t))
                         ovs)
            (throw 'found t)))
        (forward-char 1))
      nil)))

(defun presentation-start ()
  "Start presentation mode."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "Presentation mode only works in Org-mode buffers."))

  ;; Save original buffer-local settings
  (setq presentation--original-settings
        `((truncate-lines . ,truncate-lines)
          (scroll-margin . ,scroll-margin)
          (mode-line-format . ,mode-line-format)
          (header-line-format . ,header-line-format)
          (display-line-numbers . ,display-line-numbers)))

  (setq-local buffer-read-only t)

  ;; Save if images currently displayed
  (setq presentation--inline-images-shown (presentation--inline-images-shown-p))

  ;; Enable inline images
  (org-display-inline-images nil t)

  ;; ... other visual adjustments like face remapping here ...

  (setq-local truncate-lines nil)
  (setq-local scroll-margin 0)
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (setq-local display-line-numbers nil)

  ;; Collect slides and show first one
  (presentation--collect-level-1-headings)
  (if presentation--slide-headings
      (progn
        (setq presentation--current-slide-index 0)
        (presentation--goto-slide 0)

        ;; Bind keys
        (local-set-key (kbd "<right>") #'presentation-next-slide)
        (local-set-key (kbd "<left>")  #'presentation-prev-slide)
        (local-set-key (kbd "n")       #'presentation-next-slide)
        (local-set-key (kbd "p")       #'presentation-prev-slide)
        (local-set-key (kbd "q")       #'presentation-quit)
        (local-set-key (kbd "S-<f5>") #'presentation-quit)

        (message "Presentation started. Use n/p or arrows to navigate. q or ESC to quit."))
    (message "No level-1 headings found.")))

(defun presentation-quit ()
  "Quit presentation and restore buffer settings."
  (interactive)
  (widen)

  ;; Restore inline images to previous state
  (if presentation--inline-images-shown
      (org-display-inline-images nil t)
    (org-remove-inline-images))

  ;; Restore buffer-local settings
  (dolist (setting presentation--original-settings)
    (set (make-local-variable (car setting)) (cdr setting)))

  ;; Unbind keys
  (local-unset-key (kbd "<right>"))
  (local-unset-key (kbd "<left>"))
  (local-unset-key (kbd "n"))
  (local-unset-key (kbd "p"))
  (local-unset-key (kbd "q"))
  (local-unset-key (kbd "S-<f5>"))
  (setq-local buffer-read-only nil)

  (setq header-line-format nil)
  (setq presentation--slide-headings nil)
  (message "Presentation ended."))

(provide 'presentation-mode)
;;; presentation-mode.el ends here
