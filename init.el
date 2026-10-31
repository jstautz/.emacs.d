;;; init.el --- Main config -*- lexical-binding: t; -*-

;;; Paths
(defconst jcs:lisp-dir (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path jcs:lisp-dir)

;;; Customize
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;;; Packages
;; elpa/ is committed without .elc, so compile any package that lacks them
;; (fresh clone, or installed on the other machine).
(defvar byte-compile-warnings)
(let ((byte-compile-warnings '(not obsolete)))
  (pcase-dolist (`(,_ ,desc) package-alist)
    (unless (directory-files (package-desc-dir desc) nil "\\.elc\\'")
      (package-recompile desc))))

;;; Shell environment
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  ;; PATH is set in .zshrc, so this needs an interactive shell (~700ms).
  (setq exec-path-from-shell-warn-duration-millis 1500)
  (exec-path-from-shell-initialize))

;;; Server
(require 'server)
(unless (or noninteractive (server-running-p))
  (server-start))

;;; Org
(load (expand-file-name "org-mode-init" user-emacs-directory) nil 'nomessage)

;;; init.el ends here
