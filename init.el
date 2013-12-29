;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Jeff's .emacs init file
;;
;; - Sets up load paths, etc.
;; - Defines my "decrypt-secrets" function for use in other files
;; - Loads all packages (init-packages.el)
;; - Loads custom keybindings, UI, and editing prefs (init-customizations.el)
;; - Loads custom + util functions (init-custom-functions.el)
;; - Fires up Emacs server
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(setq exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/bin"
                        "/usr/X11/bin" "/opt/local/bin" "/usr/local/git/bin"
                        "/Applications/Emacs.app/Contents/MacOS/bin"
                        "/Users/jstautz/bin" "/usr/texbin")))

(setq home-dir "/Users/jstautz/"
      dotemacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
      emacs-dir "/Applications/Emacs.app/Contents/"
      custom-file (concat dotemacs-dir "init-customizations.el")
      emacs-bin (concat emacs-dir "MacOS/Emacs")
      info-dir (concat emacs-dir "Resources/info/"))

(add-to-list 'load-path dotemacs-dir)

;; Decrypt and load secrets.el.gpg file containing passwords, etc.
(defun jcs:decrypt-secrets ()
  (interactive)
  (require 'secrets))

(require 'init-packages)
(require 'init-customizations)
(require 'init-custom-functions)

(server-start)
