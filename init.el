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
;; See ~Cask~ file for its configuration
;; https://github.com/cask/cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'use-package)

;; Keeps ~Cask~ file in sync with the packages
;; that you install/uninstall via ~M-x list-packages~
;; https://github.com/rdallasgray/pallet
(use-package pallet)


(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))

;; ;; Load the necessary bits of org-mode and org-babel
;; (if (file-directory-p (concat org-mode-dir "lisp"))
;;     (progn
;;       (add-to-list 'load-path (concat org-mode-dir "lisp"))
;;       (add-to-list 'load-path (concat org-mode-dir "contrib/lisp"))
;;       (require 'org)
;;       (require 'ob-tangle)
;;       (require 'org-id))
;;   (message "Could not load org-mode! Expecting it in the el-get/org-mode directory."))

;; Load the rest of my config from org-mode file
(org-babel-load-file (expand-file-name "emacs-init.org" dotemacs-dir))

;;; init.el ends here
