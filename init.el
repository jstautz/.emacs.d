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
(setq mode-line-collapse-minor-modes '(auto-revert-mode guide-key-mode)
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
(require 'recentf)
(fido-vertical-mode 1)
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

;;; Git
(use-package magit
  :ensure t
  :defer t)

;;; Markdown
(use-package markdown-mode
  :ensure t
  :defer t)

;;; HTML export
;; used by Org for colorized source blocks in HTML export
(use-package htmlize
  :ensure t
  :defer t)

;;; Server
(require 'server)
(unless (or noninteractive (server-running-p))
  (server-start))

;;; Org
(load (expand-file-name "org-mode-init" user-emacs-directory) nil 'nomessage)

;;; init.el ends here
