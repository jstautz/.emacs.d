;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Jeff's init.el file --- Where the magic begins
;;
;; This is the first file Emacs loads.
;;
;; The only thing done in this file is to load org-mode via the ORG_HOME
;; environment variable, if set, then use org-babel to load org-mode files
;; containing the rest of my setup.
;;
;; Ideas and elisp stolen from Eric Schulte's literate fork of
;; emacs-starter-kit: https://github.com/eschulte/emacs24-starter-kit
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Ugly hack to get access to ORG_HOME env variable
(let ((jcs:org-home (shell-command-to-string ". ~/.bashrc; echo -n $ORG_HOME")))
  (setenv "ORG_HOME" jcs:org-home))

;; Load Org-mode from source when the ORG_HOME environment variable is set
(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (require 'org))))

;; Load the rest of my init from the `after-init-hook' so all packages
;; are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq dotemacs-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (getenv "ORG_HOME")
                  (file-directory-p (expand-file-name "lisp"
                                                      (getenv "ORG_HOME"))))
       '(require 'org))
    ;; load up the rest of my emacs init
    (org-babel-load-file (expand-file-name "emacs-init.org" dotemacs-dir))))

;;; init.el ends here
