;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Jeff's init.el file --- Where the magic begins
;;
;; This is the first file Emacs loads.
;;
;; The only thing done in this file is to load org-mode, then use org-babel
;; to load org-mode files containing the rest of my setup.
;;
;; Ideas and elisp stolen from Eric Schulte's literate fork of
;; emacs-starter-kit: https://github.com/eschulte/emacs24-starter-kit
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Assume current directory is the dot-emacs directory and add to load-path
(setq dotemacs-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; (add-to-list 'load-path dotemacs-dir)

;; Load cask and activate it.
;; https://github.com/cask/cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; If emacs-init.el is *newer* than emacs-init.org, then load the *.el file directly.
;; Otherwise, tangle the *.org file and load.
(if (> (string-to-int (shell-command-to-string "stat -f \"%m\" ~/.emacs.d/emacs-init.el"))
       (string-to-int (shell-command-to-string "stat -f \"%m\" ~/.emacs.d/emacs-init.org")))
      (load-file (expand-file-name "emacs-init.el" dotemacs-dir))
    (org-babel-load-file (expand-file-name "emacs-init.org" dotemacs-dir)))
  
;;; init.el ends here
