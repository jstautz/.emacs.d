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


;; Assume current directory is the dot-emacs directory.
(setq dotemacs-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Assume that org-mode is installed via el-get
;; TODO: check if this is valid?
;; TODO: this is kind of a chicken and egg problem...
(setq org-mode-dir (concat dotemacs-dir "el-get/org-mode/"))

;; Load the necessary bits of org-mode and org-babel
(if (file-directory-p (concat org-mode-dir "lisp"))
    (progn
      (add-to-list 'load-path (concat org-mode-dir "lisp"))
      (add-to-list 'load-path (concat org-mode-dir "contrib/lisp"))
      (require 'org)
      (require 'ob-tangle)
      (require 'org-id))
  (message "Could not load org-mode! Expecting it in the el-get/org-mode directory."))

;; Load the rest of my config from org-mode file
(org-babel-load-file (expand-file-name "emacs-init.org" dotemacs-dir))

;;; init.el ends here
