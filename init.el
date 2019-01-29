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

;; If emacs-init.el is *newer* than emacs-init.org, then load the *.el file directly.
;; Otherwise, tangle the *.org file and load.
(if (> (string-to-int (shell-command-to-string "stat -f \"%m\" ~/.emacs.d/lisp/emacs-init.el"))
       (string-to-int (shell-command-to-string "stat -f \"%m\" ~/.emacs.d/emacs-init.org")))
    (load-file (expand-file-name "lisp/emacs-init.elc" (file-name-directory (or load-file-name (buffer-file-name)))))
  (org-babel-load-file (expand-file-name "emacs-init.org" (file-name-directory (or load-file-name (buffer-file-name)))) t))

;; Do the same with my org-mode config -- tangle and load.
(if (> (string-to-int (shell-command-to-string "stat -f \"%m\" ~/.emacs.d/lisp/org-mode-init.el"))
       (string-to-int (shell-command-to-string "stat -f \"%m\" ~/.emacs.d/org-mode-init.org")))
    (load-file (expand-file-name "lisp/org-mode-init.elc" (file-name-directory (or load-file-name (buffer-file-name)))))
  (org-babel-load-file (expand-file-name "org-mode-init.org" (file-name-directory (or load-file-name (buffer-file-name)))) t))
