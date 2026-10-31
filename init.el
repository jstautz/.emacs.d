;;; init.el --- Main config -*- lexical-binding: t; -*-

;;; Set paths
(defconst jcs:lisp-dir (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path jcs:lisp-dir)

;;; File away customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;;; Packages
;; Compile any elpa packages that need it.
(defvar byte-compile-warnings)
(let ((byte-compile-warnings '(not obsolete)))
  (pcase-dolist (`(,_ ,desc) package-alist)
    (unless (directory-files (package-desc-dir desc) nil "\\.elc\\'")
      (package-recompile desc))))

;;; Shell environment
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  ;; PATH is set in .zshrc, so this needs an interactive shell (~700ms).
  ;; TODO update .zshrc, .profile, .zshenv files to save load time
  (setq exec-path-from-shell-warn-duration-millis 1500)
  (exec-path-from-shell-initialize))

;;; Interface
(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function #'ignore
      use-short-answers t
      confirm-kill-emacs #'y-or-n-p)
(column-number-mode 1)
(set-face-attribute 'default nil :family "Menlo" :height 180)
;; unspecified = use the frame background
(set-face-attribute 'fringe nil :background 'unspecified)

;;; Keys
;; use <esc> as quit vs meta prefix
(keymap-global-set "<escape>" #'keyboard-escape-quit)
(keymap-global-set "C-<escape>" #'top-level)
;; quit deliberately, via M-x save-buffers-kill-emacs
(keymap-global-unset "C-x C-c")

;;; Server
(require 'server)
(unless (or noninteractive (server-running-p))
  (server-start))

;;; Org
(load (expand-file-name "org-mode-init" user-emacs-directory) nil 'nomessage)

;;; init.el ends here
