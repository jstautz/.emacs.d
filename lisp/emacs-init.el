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

;; Assume current directory is the dot-emacs directory
(add-to-list 'load-path (concat dotemacs-dir "lisp/"))

;; Set some other useful environment vars, specific to my setup
(setq home-dir "/Users/jeff.stautz/"
      emacs-dir "/Applications/Emacs.app/Contents/"
      emacs-bin (concat emacs-dir "MacOS/Emacs")
      info-dir (concat emacs-dir "Resources/info/"))

;; Make my $PATH environment var the same as in bash
(let ((jcs:shell-path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" jcs:shell-path)
  (setq exec-path (split-string jcs:shell-path ":")))

;; Bootstrap & install packages:
(load-file (concat dotemacs-dir "lisp/packages.el"))

;; Set up interface and editor options the way I like 'em:
(load-file (concat dotemacs-dir "lisp/settings.el"))

(load-file "~/.emacs.d/lisp/init-org-mode.el")
