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

(add-to-list 'load-path dotemacs-dir)

;; Package Manager
;; https://github.com/cask/cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(org-babel-load-file (expand-file-name "emacs-init.org" dotemacs-dir))

;;; init.el ends here
