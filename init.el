;;; package --- Summary
;;; Commentary:
;;; Code:

;;(package-initialize)

;; define  variables by using a macro that generates and evaluates as below
;; (defconst EMACS24+   (> emacs-major-version 23)) using macros
(defmacro yae-gen-ver-consts (var)
  "Define a macro to create constants to check Emacs version greater than VAR."
  (list 'defconst
        (intern (concat "EMACS" (int-to-string var) "+"))
        (> emacs-major-version (- var 1))))

;;evaluate the macro for the number range we want
(seq-do (lambda (x) (eval (list 'yae-gen-ver-consts x))) (number-sequence 24 35))

;; (map #(eval (list 'yae-gen-ver-consts %)) (range 24 36))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))


(setq vc-follow-symlinks t)
(let ((file-name-handler-alist nil))
  (require 'package)

  ;; in ~/.emacs.d/init.el (or ~/.emacs.d/early-init.el in Emacs 27)
  (setq package-enable-at-startup nil ; don't auto-initialize!
        ;; don't add that `custom-set-variables' block to my init.el!
        package--init-file-ensured t)


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
