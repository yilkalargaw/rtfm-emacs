;;; package --- Summary
;;; Commentary:
;;; Code:

;;(package-initialize)

(setq vc-follow-symlinks t)

(let ((file-name-handler-alist nil))
  (require 'package)

  ;; in ~/.emacs.d/init.el (or ~/.emacs.d/early-init.el in Emacs 27)
  (setq package-enable-at-startup nil ; don't auto-initialize!
        ;; don't add that `custom-set-variables' block to my init.el!
        package--init-file-ensured t)

  ;; ;; define custom files
  ;; (defcustom yae-init-org-file-list
  ;;   '("rtfm.org"
  ;;     "server.org"
  ;;     "pluggedin.org"
  ;;     )
  ;;   "List of Org files to load at startup."
  ;;   :type '(repeat string)
  ;;   :group 'my-customizations)

  ;; ;; define load function
  ;; (defun load-yae-init-files ()
  ;;   "Load all Org files specified in `org-file-list`."
  ;;   (dolist (file yae-init-org-file-list)
  ;;     (org-babel-load-file
  ;;      (expand-file-name file user-emacs-directory))))

  ;; ;; Call the function to load the files
  ;; (load-yae-init-files)

  (defvar my-config-org-files
    (mapcar (lambda (file) (expand-file-name file user-emacs-directory))
            '("rtfm.org"
              ;; "server.org"
              ;; "pluggedin.org"
              )))

  (defvar my-compiled-config-dir (expand-file-name "compiled/" user-emacs-directory))

  (defun my-byte-compile-config ()
    "Compile all org config files to elisp and byte-compile them."
    (interactive)
    (make-directory my-compiled-config-dir t)
    (dolist (org-file my-config-org-files)
      (let* ((base-name (file-name-nondirectory org-file))
             (el-file (expand-file-name (replace-regexp-in-string "\\.org$" ".el" base-name)
                                        my-compiled-config-dir))
             (elc-file (expand-file-name (replace-regexp-in-string "\\.org$" ".elc" base-name)
                                         my-compiled-config-dir)))
        
        ;; Only recompile if needed
        (when (or (not (file-exists-p elc-file))
                  (file-newer-than-file-p org-file elc-file))
          ;; Export Org to Elisp
          (require 'ox)
          (with-current-buffer (find-file-noselect org-file)
            (org-babel-tangle-file org-file el-file))
          
          ;; Byte-compile the generated Elisp
          (byte-compile-file el-file)
          
          (message "Compiled %s -> %s" org-file elc-file)))))

  ;; Improved config loader that uses compiled files
  (defun my-load-compiled-config ()
    "Load the byte-compiled configuration files."
    (dolist (org-file my-config-org-files)
      (let ((elc-file (expand-file-name
                       (replace-regexp-in-string "\\.org$" ".elc"
                                                 (file-name-nondirectory org-file))
                       my-compiled-config-dir)))
        (when (file-exists-p elc-file)
          (load elc-file t t)))))  ; t t for noerror nomessage

    ;; Setup hooks
    (add-hook 'after-init-hook 'my-byte-compile-config)
    (add-hook 'after-save-hook
              (lambda ()
                (when (member (expand-file-name (buffer-file-name)) my-config-org-files)
                  (my-byte-compile-config))))

    ;; Load the compiled config at startup
    (my-load-compiled-config)


(add-hook 'after-init-hook
          (lambda ()
            (let ((dafile (expand-file-name "custom_faces.el" user-emacs-directory)))
              (when (file-exists-p dafile) (load-file dafile)))))

(when EMACS27+
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (let ((dafile (expand-file-name "custom_faces.el" user-emacs-directory)))
                (when (file-exists-p dafile) (load-file dafile))))))


;; ------------------------------------------------------------
;; Benchmarking Helpers
;; ------------------------------------------------------------

(defun my/emacs-startup-time ()
  "Display the startup time in the minibuffer."
  (interactive)
  (message "Emacs started in %.2f seconds"
           (float-time (time-subtract after-init-time before-init-time))))

;; Add a hook to display startup time
(add-hook 'emacs-startup-hook #'my/emacs-startup-time)

(provide 'init))
;;; init.el ends here
