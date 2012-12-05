;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Jeff's .emacs init file
;;
;; - Sets up load paths, etc.
;; - Loads all packages (init-packages.el)
;; - Sets up org-mode the way I like it (init-org.el)
;; - Loads custom keybindings, UI, and editing prefs (init-customizations.el)
;; - Loads custom + util functions (init-custom-functions.el)
;; - Fires up Emacs server and goes fullscreen
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(defvar *emacs-load-start* (current-time))

(setq exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/bin"
                        "/usr/X11/bin" "/opt/local/bin" "/usr/local/git/bin"
                        "/Applications/Emacs.app/Contents/MacOS/bin"
                        "/Users/jstautz/bin")))

(setq home-dir "/Users/jstautz/"
      dotemacs-dir (file-name-directory (or
      load-file-name (buffer-file-name)))
      emacs-dir "/Applications/Emacs.app/Contents/"
      custom-file (concat dotemacs-dir "init-customizations.el")
      emacs-bin (concat emacs-dir "MacOS/Emacs") info-dir (concat
      emacs-dir "Resources/info/"))

(add-to-list 'load-path dotemacs-dir)

(require 'init-packages)
(require 'init-customizations)
(require 'init-custom-functions)

(server-start)

;; NOTE: fullscreen requires patched Emacs 24
;; See https://gist.github.com/1397155
(ns-toggle-fullscreen)

(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                             (- (+ hi lo) (+ (first *emacs-load-start*)
                                             (second *emacs-load-start*)))))


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
