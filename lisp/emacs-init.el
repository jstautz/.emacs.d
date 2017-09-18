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
