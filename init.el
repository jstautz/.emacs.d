;;; init.el --- Main config -*- lexical-binding: t; -*-

;;; Set paths
(defconst jcs:lisp-dir (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path jcs:lisp-dir)

;;; File away customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;;; Performance
;; GC every 20MB allocated instead of the 0.8MB default
(setq gc-cons-threshold 20000000)

;;; Packages
;; Compile any elpa packages that need it.
(defvar byte-compile-warnings)
(let ((byte-compile-warnings nil))
  (pcase-dolist (`(,_ ,desc) package-alist)
    (unless (directory-files (package-desc-dir desc) nil "\\.elc\\'")
      (package-recompile desc))))
;; prefer GNU/NonGNU ELPA when a package is in more than one archive
;; (use-package's :ensure loads package.el anyway)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("gnu" . 3) ("nongnu" . 2) ("melpa" . 1)))

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

;;; Backups & trash
;; keep backups and auto-saves out of working directories
(setq backup-directory-alist '((".*" . "~/.emacs.backup/"))
      auto-save-file-name-transforms '((".*" "~/.emacs.autosave/" t))
      vc-make-backup-files t
      ;; no trash-directory: nil uses the macOS Trash, so Finder's "Put Back" works
      delete-by-moving-to-trash t)
;; Emacs creates the backup dir on demand, but not the auto-save dir
(make-directory "~/.emacs.autosave/" t)

;;; Cursor & scrolling
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil :background "grey93")
;; never recenter when point moves off screen; keep point's screen row when paging
(setq scroll-conservatively 1000
      scroll-preserve-screen-position t)
;; scroll the view without moving point
(keymap-global-set "M-n" #'scroll-up-line)
(keymap-global-set "M-p" #'scroll-down-line)
(delete-selection-mode 1)
;; CUA only for rectangles: C-RET starts one; no C-x/C-c/C-v hijacking
(setopt cua-enable-cua-keys nil)
(cua-mode 1)

;;; Mode line
;; hide these minor-mode lighters (built-in replacement for diminish)
(setq mode-line-collapse-minor-modes '(auto-revert-mode guide-key-mode visual-line-mode)
      mode-line-collapse-minor-modes-to "")

;;; Keys
;; use <esc> as quit vs meta prefix
(keymap-global-set "<escape>" #'keyboard-escape-quit)
(keymap-global-set "C-<escape>" #'top-level)
;; quit deliberately, via M-x save-buffers-kill-emacs
(keymap-global-unset "C-x C-c")
;; keys I hit by accident (C-x C-q read-only, C-x . fill prefix, M-` menu bar)
(keymap-global-unset "C-x C-q")
(keymap-global-unset "<f2>")
(keymap-global-unset "C-x .")
(keymap-global-unset "M-`")
;; muscle memory: Cmd-s saves; replaces the M-s search prefix
(keymap-global-set "M-s" #'save-buffer)
(keymap-global-set "C-z" #'undo)

;;; Mac
;; both Command and Option are Meta
(setq ns-command-modifier 'meta
      ns-alternate-modifier 'meta)

;;; Completion
;; fido = flex matching, case-insensitive; savehist keeps M-x history sorting across restarts
(require 'icomplete)
(require 'recentf)
(fido-vertical-mode 1)
;; ido-style TAB: fill in the selected candidate without exiting
(keymap-set icomplete-fido-mode-map "TAB" #'icomplete-force-complete)

;; ido-style C-f at end of input: turn fido off in this prompt only, so
;; RET takes exactly what you typed
(defun jcs:fido-fallback ()
  "Move forward, or at end of input, turn off fido for this prompt."
  (interactive)
  (if (not (eolp))
      (forward-char)
    (remove-hook 'post-command-hook #'icomplete-post-command-hook t)
    (delete-overlay icomplete-overlay)
    ;; fido's keymaps are layered over the prompt's own; peel them off
    (let ((map (current-local-map))
          (layers (list icomplete-minibuffer-map icomplete-fido-mode-map
                        icomplete-vertical-mode-minibuffer-map)))
      (while (seq-some (lambda (layer) (memq layer map)) layers)
        (setq map (keymap-parent map)))
      (use-local-map map))
    (kill-local-variable 'completion-styles)
    (message "fido off for this prompt")))
(keymap-set icomplete-fido-mode-map "C-f" #'jcs:fido-fallback)
(recentf-mode 1)
(savehist-mode 1)

;; ido-style virtual buffers: C-x b also offers recently closed files
(defun jcs:switch-to-buffer-or-recent-file ()
  "Switch to a buffer, or visit a recently closed file."
  (interactive)
  (let* ((buffers (seq-remove (lambda (name) (string-prefix-p " " name))
                              (mapcar #'buffer-name
                                      (delq (current-buffer) (buffer-list)))))
         (files (mapcar #'abbreviate-file-name
                        (seq-remove #'get-file-buffer recentf-list)))
         (candidates (append buffers files))
         ;; keep recency order: buffers first, then recent files
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity)
                                 (cycle-sort-function . identity))
                    (complete-with-action action candidates string pred))))
         (choice (completing-read "Switch to buffer: " table nil nil nil
                                  'buffer-name-history)))
    (if (and (member choice files) (not (get-buffer choice)))
        (find-file choice)
      (switch-to-buffer choice))))
(keymap-global-set "C-x b" #'jcs:switch-to-buffer-or-recent-file)

;;; Windows
(keymap-global-set "M-o" #'other-window)

;; new split shows the previous buffer, not the same one twice
(defun jcs:split-below-last-buffer ()
  "Split the window below and show the previous buffer there."
  (interactive)
  (select-window (split-window-below))
  (switch-to-next-buffer))

(defun jcs:split-right-last-buffer ()
  "Split the window right and show the previous buffer there."
  (interactive)
  (select-window (split-window-right))
  (switch-to-next-buffer))

(keymap-global-set "C-x 2" #'jcs:split-below-last-buffer)
(keymap-global-set "C-x 3" #'jcs:split-right-last-buffer)

;; zoom one window full-frame and back; the split is kept in register u
(defun jcs:toggle-windows-split ()
  "Toggle between one window and the previous split of windows."
  (interactive)
  (unless (window-minibuffer-p)
    (if (< 1 (count-windows))
        (progn
          (window-configuration-to-register ?u)
          (delete-other-windows))
      (jump-to-register ?u))))

(keymap-global-set "C-`" #'jcs:toggle-windows-split)
(keymap-global-set "C-~" #'jcs:toggle-windows-split)

;; resize windows from the keyboard (overrides C-M-<arrow> sexp motion)
(keymap-global-set "C-M-<left>" #'enlarge-window-horizontally)
(keymap-global-set "C-M-<right>" #'shrink-window-horizontally)
(keymap-global-set "C-M-<up>" #'enlarge-window)
(keymap-global-set "C-M-<down>" #'shrink-window)

;;; Files & buffers
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "C-x C-r" #'rename-visited-file)
;; reload files changed outside Emacs; buffers with unsaved edits are left alone
(global-auto-revert-mode 1)

(defun jcs:delete-current-buffer-file ()
  "Delete the file visited by this buffer, then kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename t)
        (kill-buffer)
        (message "File '%s' successfully removed" filename)))))
;; replaces the C-x C-k keyboard-macro prefix (F3/F4 still record/play)
(keymap-global-set "C-x C-k" #'jcs:delete-current-buffer-file)

;; M-x locate uses Spotlight
(setopt locate-make-command-line (lambda (s) (list "mdfind" "-name" s)))
(setq large-file-warning-threshold 100000000)

;;; Dired
(use-package dired
  :defer t
  :functions (dired-next-line dired-goto-next-nontrivial-file dired-do-open)
  :preface
  (defun jcs:dired-back-to-top ()
    "Move to the first file, skipping . and .."
    (interactive)
    (goto-char (point-min))
    (dired-next-line 1)
    (dired-goto-next-nontrivial-file))
  (defun jcs:dired-jump-to-bottom ()
    "Move to the last file."
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))
  :config
  ;; macOS ls has no --dired; saying so up front skips the warning
  (setopt dired-use-ls-dired nil)
  (keymap-set dired-mode-map "r" #'wdired-change-to-wdired-mode)
  ;; open in the default macOS app
  (keymap-set dired-mode-map "z" #'dired-do-open)
  (keymap-set dired-mode-map "<remap> <beginning-of-buffer>" #'jcs:dired-back-to-top)
  (keymap-set dired-mode-map "<remap> <end-of-buffer>" #'jcs:dired-jump-to-bottom))

;;; Desktop
;; buffers, windows, registers and search rings; savehist covers minibuffer histories
(desktop-save-mode 1)
;; load a locked desktop only if the Emacs that locked it is gone
(setopt desktop-load-locked-desktop 'check-pid)

;;; Popups
(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

;;; Jumping
;; C-. then a character: jump to any word starting with it (ace-jump style)
(use-package avy
  :ensure t
  :bind ("C-." . avy-goto-word-1))

;;; Kill ring
;; M-y without a preceding C-y opens the browser
(use-package browse-kill-ring
  :ensure t
  :functions popwin:update-window-reference
  :preface
  ;; popwin replaces the window browse-kill-ring yanks into; point it at the new one
  (defun jcs:popwin-update-kill-ring-window ()
    (popwin:update-window-reference 'browse-kill-ring-original-window :safe t))
  :config
  (browse-kill-ring-default-keybindings)
  (push "*Kill Ring*" popwin:special-display-config)
  (add-hook 'popwin:after-popup-hook #'jcs:popwin-update-kill-ring-window))

;;; Key hints
(use-package guide-key
  :ensure t
  :functions guide-key/add-local-guide-key-sequence
  :preface
  (defun jcs:guide-key-org-sequences ()
    (guide-key/add-local-guide-key-sequence "C-c")
    (guide-key/add-local-guide-key-sequence "C-c C-x"))
  :hook (org-mode . jcs:guide-key-org-sequences)
  ;; :hook alone defers loading, so guide-key-mode would never start
  :demand t
  :config
  (setq guide-key/idle-delay 1
        guide-key/popup-window-position 'bottom)
  (guide-key-mode 1))

;;; Emacs Lisp
(defun jcs:eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)) t) (current-buffer)))
(keymap-global-set "C-c e" #'jcs:eval-and-replace)

;;; Web
(use-package web-mode
  :ensure t
  :defer t)

;;; REST
;; M-x restclient-mode in a buffer of HTTP requests; C-c C-c sends one
(use-package restclient
  :ensure t
  :defer t)

;;; Git
(use-package magit
  :ensure t
  :defer t)

;;; Markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :config
  ;; dropping a file opens it instead of inserting a link
  (setopt markdown-yank-dnd-method 'open))

;;; HTML export
;; used by Org for colorized source blocks in HTML export
(use-package htmlize
  :ensure t
  :defer t)

;;; Writing
(setq-default fill-column 120
              indent-tabs-mode nil
              tab-width 4)
(setq sentence-end-double-space nil)
(setopt doc-view-continuous t)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; soft-wrap text at fill-column, centered in the window
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)
(use-package visual-fill-column
  :ensure t
  :hook (visual-line-mode . visual-fill-column-mode)
  :init
  (setq-default visual-fill-column-center-text t))

;; M-x unfill-paragraph is built in as of Emacs 31

;;; Spelling
;; needs `brew install aspell'; flyspell stays off until it's on PATH
(setopt ispell-program-name "aspell"
        ispell-dictionary "en_CA")
(setq ispell-personal-dictionary "~/Dropbox/.spell.en.pws")
(when (executable-find "aspell")
  (add-hook 'text-mode-hook #'flyspell-mode))
(use-package flyspell
  :defer t
  :config
  (keymap-set flyspell-mode-map "C-;" #'ispell-word)
  ;; leave C-. to avy (flyspell binds it to auto-correct)
  (keymap-unset flyspell-mode-map "C-.")
  ;; C-c $ is org-archive-subtree
  (keymap-unset flyspell-mode-map "C-c $"))

;;; Server
(require 'server)
(unless (or noninteractive (server-running-p))
  (server-start))

;;; Org
(load (expand-file-name "org-mode-init" user-emacs-directory) nil 'nomessage)

;;; init.el ends here
