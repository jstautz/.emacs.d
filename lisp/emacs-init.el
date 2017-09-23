(dolist (source '( ("gnu"   . "http://elpa.gnu.org/packages/")
                   ("elpa"  . "http://tromey.com/elpa/")
		         ("org"   . "http://orgmode.org/elpa/")
                   ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package cl)

(let ((jcs:shell-path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" jcs:shell-path)
  (setq exec-path (split-string jcs:shell-path ":")))

(defvar home-dir     "/Users/jeff.stautz/")
(defvar dotemacs-dir (concat home-dir ".emacs.d/"))
(defvar lisp-dir     (concat dotemacs-dir "lisp/"))
(defvar emacs-dir    "/Applications/Emacs.app/Contents/")
(defvar emacs-bin    (concat emacs-dir "MacOS/Emacs"))
(defvar info-dir     (concat emacs-dir "Resources/info/"))

(add-to-list 'load-path lisp-dir)

(setq custom-file (concat lisp-dir "custom.el"))

(setq gc-cons-threshold 20000000)

(defun jcs:decrypt-secrets ()
  (interactive)
  (require 'secrets))

;; Bootstrap & install packages:
(load-file (concat dotemacs-dir "lisp/packages.el"))

;; Set up interface and editor options the way I like 'em:
(load-file (concat dotemacs-dir "lisp/settings.el"))

(load-file "~/.emacs.d/lisp/init-org-mode.el")
