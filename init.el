;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Jeff's init.el file --- Where the magic begins
;;
;; This is the first file Emacs loads.
;;
;; It doesn't do much, just:
;; - Initializes packages 
;; - Defines my .emacs directory
;; - Use org-babel to bootstrap the rest of my configuration from org-files
;;
;; Ideas and elisp stolen from Eric Schulte's literate fork of
;; emacs-starter-kit: https://github.com/eschulte/emacs24-starter-kit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

(org-babel-load-file (concat user-emacs-directory "emacs-init.org"))
