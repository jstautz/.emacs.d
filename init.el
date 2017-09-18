;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Jeff's .emacs init file
;;
;; - Set up custom package sources
;; - Define directory variables
;; - Set up my $PATH
;; - Load separate files to set up packages, interface options, and org-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Set up package sources
(require 'package)
(dolist (source '( ("gnu" . "http://elpa.gnu.org/packages/")
                   ("elpa" . "http://tromey.com/elpa/")
		   ("org"         . "http://orgmode.org/elpa/")
                   ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

;; Assume current directory is the dot-emacs directory
(setq dotemacs-dir (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'load-path (concat dotemacs-dir "lisp/"))

;; Set some other useful environment vars, specific to my setup
(setq home-dir "/Users/jeff.stautz/"
      emacs-dir "/Applications/Emacs.app/Contents/"
      emacs-bin (concat emacs-dir "MacOS/Emacs")
      info-dir (concat emacs-dir "Resources/info/"))

;; Gross hack to make my $PATH environment var the same as in bash
(let ((jcs:shell-path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" jcs:shell-path)
  (setq exec-path (split-string jcs:shell-path ":")))


;; For the sake of this demo, I'm splitting this into separate .el files

;; Bootstrap & install packages:
(load-file (concat dotemacs-dir "lisp/packages.el"))

;; Set up interface and editor options the way I like 'em:
(load-file (concat dotemacs-dir "lisp/settings.el"))

;; And now, load org-mode and its settings:
;;(load-file (concat dotemacs-dir "lisp/org-mode.el"))
(load-file "~/Desktop/emacs.d/config/init-org-mode.el")
