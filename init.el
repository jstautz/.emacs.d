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

;; Assume current directory is our dot-emacs directory
(setq dotemacs-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; If emacs-init.el is *newer* than emacs-init.org, then load the *.el file directly.
;; Otherwise, tangle the *.org file and load.
(if (> (string-to-int (shell-command-to-string "stat -f \"%m\" ~/.emacs.d/lisp/emacs-init.el"))
       (string-to-int (shell-command-to-string "stat -f \"%m\" ~/.emacs.d/emacs-init.org")))
      (load-file (expand-file-name "lisp/emacs-init.el" dotemacs-dir))
  (org-babel-load-file (expand-file-name "emacs-init.org" dotemacs-dir)))
