;;------------------------------------------------------------------------------
;;
;; Interface and editor options I like
;;
;;------------------------------------------------------------------------------

;; Get rid of all that chrome and fuss
(tool-bar-mode -1)
(set-face-background 'fringe (face-background 'default))
(set-face-foreground 'fringe (face-background 'default))
(scroll-bar-mode -1)

;; Don't talk to me
(setq inhibit-splash-screen 1)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq ring-bell-function (lambda ()))
(fset 'yes-or-no-p 'y-or-n-p)

;; Go faster
(setq redisplay-dont-pause t)
(setq gc-cons-threshold 20000000)

;; Don't let me quit on accident
(global-unset-key "\C-x\C-c")
(setq confirm-kill-emacs 'y-or-n-p)

;; I'm a Mac
(setq ns-alternate-modifier (quote meta))
(setq ns-command-modifier (quote meta))
(setq mac-emulate-three-button-mouse t)
(define-key global-map [ns-drag-file] 'ns-find-file) 
(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))
(setq x-select-enable-clipboard t)

;; Show me where I am
(line-number-mode 1)
(column-number-mode 1)
(setq blink-cursor-mode t)
(global-hl-line-mode 1)
(set-face-background 'hl-line "grey93")

;; Word wrap and center my text nicely
(setq fill-column 120)
(setq default-fill-column 120)
(load-file "~/.emacs.d/lisp/wrap-to-fill.el")
(visual-line-mode 1)
(wrap-to-fill-column-mode 1)
(add-hook 'text-mode-hook '(lambda() (wrap-to-fill-column-mode 1)))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(diminish 'visual-line-mode)
(diminish 'wrap-to-fill-column-mode)

;; Don't scroll like a maniac
(setq scroll-conservatively 1000)
(setq scroll-margin 0)
(setq scroll-up-aggressively nil
      scroll-down-aggressively nil
      scroll-preserve-screen-position t)

;; Respect the power of my mouse wheel, margins!
(global-set-key (kbd "<left-margin><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<left-margin><wheel-up>") 'mwheel-scroll)
(global-set-key (kbd "<right-margin><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<right-margin><wheel-up>") 'mwheel-scroll)
(global-set-key (kbd "<mode-line><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<mode-line><wheel-up>") 'mwheel-scroll)

;; Scroll around the cursor
(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)

;; Select text in a non-psychopathic way
(transient-mark-mode t)
(delete-selection-mode t)

;; Enable rectangle editing, but not all the other CUA hoo-hah
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; Preserve indentation
(global-set-key (kbd "RET") 'newline-and-indent)

;; Trust me, I'm an interior decorator
(setq custom-safe-themes t)

;; Don't crap up my working directory with backups
(defvar backup-dir "~/.emacs.backup/")
(defvar autosave-dir "~/.emacs.autosave/")
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; I'm paranoid
(setq vc-make-backup-files t)
(setq delete-by-moving-to-trash t)
(setq trash-directory (concat "~/.Trash/"))

;; Unset some defaults I don't like
(global-unset-key "\C-x\C-q")
(global-unset-key (kbd "<f2>"))
(global-unset-key "\C-x.")
(global-unset-key "\M-`")

;; Forgive my muscle memory
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "C-z") 'undo)

;; Escape is my eject button
(global-set-key (kbd "<C-escape>") 'top-level)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(add-hook 'org-mode-hook
          (lambda()
            (define-key org-mode-map (kbd "<escape>") 'keyboard-escape-quit)))

;; Arr, here be my custom file
(setq custom-file (concat dotemacs-dir "lisp/custom.el"))

;; I love ibuffer
(global-set-key "\C-x\C-b" 'ibuffer)

;; I love ispell
(setq ispell-program-name "aspell")
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") #'ispell-word))
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-.") nil))

;; Also, I'm Canadian.
(defvar ispell-local-dictionary-alist
  '(("canadian"
     "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1)))
(setq ispell-local-dictionary "canadian")

;; Save my place
(desktop-save-mode 1)
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))
(setq desktop-load-locked-desktop t)
(desktop-read)

;; Take off some guard rails
(put 'narrow-to-region 'disabled nil)

;; Let me jump between a split frame and single-window view
(defun toggle-windows-split()
  "Switch back and forth between one window and whatever split of
windows we might have in the frame. The idea is to maximize the
current buffer, while being able to go back to the previous split
of windows in the frame simply by calling this command again."
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
      (progn
        (if (< 1 (count-windows))
            (progn
              (window-configuration-to-register ?u)
              (delete-other-windows))
          (jump-to-register ?u)))))
(define-key global-map (kbd "C-`") 'toggle-windows-split)
(define-key global-map (kbd "C-~") 'toggle-windows-split)

;; Diminish some things
(diminish 'auto-revert-mode)

;; I keep my secret keys in here
(defun jcs:decrypt-secrets ()
  (interactive)
  (require 'secrets))

;; Send my notifications using terminal-notifier
(defvar terminal-notifier-command (executable-find "terminal-notifier") "The path to terminal-notifier.")

(defun terminal-notifier-notify (title message)
  "Show a message with `terminal-notifier-command`."
  (start-process "terminal-notifier"
                 "*terminal-notifier*"
                 terminal-notifier-command
                 "-title" title
                 "-message" message
                 "-activate" "org.gnu.Emacs"
                 "-sender" "org.gnu.Emacs"
		 "-timeout" "5"))

