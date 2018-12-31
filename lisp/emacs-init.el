;; set package sources
(dolist (source '( ("gnu"   . "http://elpa.gnu.org/packages/")
                   ("elpa"  . "http://tromey.com/elpa/")
		         ("org"   . "http://orgmode.org/elpa/")
                   ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

;; install use-package, which my setup relies on
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package cl)

;; ensure OS X keeps my path consistent in Emacs
(let ((jcs:shell-path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" jcs:shell-path)
  (setq exec-path (split-string jcs:shell-path ":")))

;; fantastic directories and where to find them
  (defvar home-dir     "/Users/jeff.stautz/")
  (defvar dotemacs-dir (concat home-dir ".emacs.d/"))
  (defvar lisp-dir     (concat dotemacs-dir "lisp/"))
  (defvar emacs-dir    "/Applications/Emacs.app/Contents/")
  (defvar emacs-bin    (concat emacs-dir "MacOS/Emacs"))
  (defvar info-dir     (concat emacs-dir "Resources/info/"))

(add-to-list 'load-path lisp-dir)

;; Arr, here be my custom file
(setq custom-file (concat lisp-dir "custom.el"))

;; build a giant garbage compactor
(setq gc-cons-threshold 20000000)

(defun jcs:decrypt-secrets ()
  (interactive)
  (require 'secrets))

;; jump around, House of Pain style
(use-package ace-jump-mode
             :bind ("C-." . ace-jump-mode))

;; show me the graveyard with M-y
(use-package browse-kill-ring
             :defer t
             :init
             (progn
               (autoload 'browse-kill-ring-default-keybindings "browse-kill-ring")
               (browse-kill-ring-default-keybindings)))

;; use fewer letters
(use-package diminish)

;; unlock the magic
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-everywhere t
      completion-ignore-case t           
      read-file-name-completion-ignore-case t
      ido-max-prospects 20
      ido-use-faces t)

(setq ido-record-ftp-work-directories nil
      ido-enable-tramp-completion nil
      ido-is-tramp-root nil)

(recentf-mode t)
(setq ido-use-virtual-buffers t)

(setq ido-auto-merge-work-directories-length -1
      ido-show-dot-for-dired t
      ido-use-filename-at-point 'guess)

(put 'ido-exit-minibuffer 'disabled nil)

;; use ido everywhere
(use-package ido-completing-read+)
(ido-ubiquitous-mode 1)

;; stack my completions
(use-package ido-vertical-mode
             :init
             (progn (ido-vertical-mode 1)
                    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))

;;smexy smex
(use-package smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; gimme some hints
(use-package guide-key)
(defun guide-key/jcs-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x"))
(add-hook 'org-mode-hook 'guide-key/jcs-hook-function-for-org-mode)
(setq guide-key/idle-delay 1)
(setq guide-key/popup-window-position 'bottom)
(guide-key-mode 1)
(diminish 'guide-key-mode)

;; put those hints in a nice pop-up window
(use-package popwin)
(defun popwin-bkr:update-window-reference ()
  (popwin:update-window-reference 'browse-kill-ring-original-window :safe t))
(add-hook 'popwin:after-popup-hook 'popwin-bkr:update-window-reference)
(push "*Kill Ring*" popwin:special-display-config)
(popwin-mode 1)

;; git on up
(use-package magit)
(setq magit-push-always-verify nil)

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

(global-unset-key "\C-x\C-c")
(setq confirm-kill-emacs 'y-or-n-p)

;; Trust me, I'm an interior decorator
(setq custom-safe-themes t)

;; whose line is it, anyway?
(line-number-mode 1)                         
(column-number-mode 1)

;; Don't crap up my working directory with backups
(defvar backup-dir "~/.emacs.backup/")
(defvar autosave-dir "~/.emacs.autosave/")
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; I'm paranoid
(setq vc-make-backup-files t)

;; Super paranoid
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

;; I'm a Mac
(setq ns-alternate-modifier (quote meta))
(setq ns-command-modifier (quote meta))

(setq x-select-enable-clipboard t)

(setq mac-emulate-three-button-mouse t)

;; show me where I am
(setq blink-cursor-mode t)

(global-hl-line-mode 1)
(set-face-background 'hl-line "grey93")

;; Don't scroll like a maniac, pls
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

(setq bookmark-default-file (concat dotemacs-dir "bookmarks"))

;; because I don't use set-face and C-x o is just too much
(global-set-key (kbd "M-o") 'other-window)

;; don't show me the same buffer twice when I split
(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )
(defun hsplit-last-buffer ()
  (interactive)
   (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

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

;; let me resize windows mouse-free
(define-key global-map (kbd "C-M-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "C-M-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "C-M-<up>") 'enlarge-window)
(define-key global-map (kbd "C-M-<down>") 'shrink-window)

;; I heart ibuffer
(global-set-key "\C-x\C-b" 'ibuffer)

;; bulk-edit files in dired
(eval-after-load 'dired
  '(define-key dired-mode-map "r"
     'wdired-change-to-wdired-mode))

;; open outside Emacs (blasphemy!)
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (start-process "default-app" nil "open" fn))))))

;; dired needs a ceiling and a floor
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(eval-after-load 'dired
  '(define-key dired-mode-map
    (vector 'remap 'beginning-of-buffer) 'dired-back-to-top))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(eval-after-load 'dired
  '(define-key dired-mode-map
    (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom))

;; rename a file in place
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; nuke this file (scream emoji)
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; don't do dumb things
(define-key global-map [ns-drag-file] 'ns-find-file)

;; search with a spotlight
(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))

;; I've got the power
(defun tramp-sudo-reopen ()
  "Re-open the current with tramp."
  (interactive)
  (let ((file-name (format "/sudo:localhost:%s" (buffer-file-name)))
        (line (line-number-at-pos))
        (column (current-column)))
    (kill-buffer)
    (find-file file-name)
    (goto-line line)
    (goto-char (+ (point) column))))

;; terminal-notifier-notify is my messenger god
(defvar terminal-notifier-command (executable-find "terminal-notifier") "The path to terminal-notifier.")

(defun terminal-notifier-notify (title message)
  "Show a message with `terminal-notifier-command`."
  (start-process "terminal-notifier"
                 "*terminal-notifier*"
                 terminal-notifier-command
                 "-title" title
                 "-message" message
                 "-activate" "org.gnu.Emacs"))

;; Set up interface and editor options the way I like 'em:
(load-file (concat dotemacs-dir "lisp/settings.el"))

;; just my size
(setq fill-column 120)
(setq default-fill-column 120)

;; float my text in the middle, all pretty-like
(load-file "~/.emacs.d/lisp/wrap-to-fill.el")
(visual-line-mode 1)
(wrap-to-fill-column-mode 1)
(add-hook 'text-mode-hook '(lambda() (wrap-to-fill-column-mode 1)))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; hide that nonsense from the mode-line
(diminish 'visual-line-mode)
(diminish 'wrap-to-fill-column-mode)

;; fables of the reconstruction
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and mashes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; herein I set down fundamental laws of nature
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq sentence-end-double-space nil)

(setq eol-mnemonic-mac "(Mac)")

;; I love ispell
(setq ispell-program-name "aspell")

;; sorry, I'm Canadian
(defvar ispell-local-dictionary-alist
  '(("canadian"
     "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1)))
(setq ispell-local-dictionary "canadian")

;; fly, you fools
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))

(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") #'ispell-word))
(eval-after-load "minor-mode"
  '(define-key flyspell-mode-map (kbd "C-c $") nil))

;; rocks ispell just shouldn't look under:
(add-to-list 'ispell-skip-region-alist '("^#\\+begin_src ". "#\\+end_src$"))
(add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC ". "#\\+END_SRC$"))

(add-to-list 'ispell-skip-region-alist '("^#\\+begin_example ". "#\\+end_example$"))
(add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE ". "#\\+END_EXAMPLE$"))

(add-to-list 'ispell-skip-region-alist '("\:PROPERTIES\:$" . "\:END\:$"))

(load-file "~/.emacs.d/lisp/init-org-mode.el")
