;;; init.el --- Main config -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Set paths
(defconst jcs:lisp-dir (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path jcs:lisp-dir)

;;;; Set separate customize file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;;;; Performance
;; GC every 20MB allocated instead of the 0.8MB default
(setq gc-cons-threshold 20000000)

;;;; Packages
;; Compile any elpa packages that need it. Supress upstream code warnings.
(defvar byte-compile-warnings)
(defvar warning-minimum-level)
(defvar warning-minimum-log-level)
(let ((byte-compile-warnings nil)
      (warning-minimum-level :error)
      (warning-minimum-log-level :error))
  (pcase-dolist (`(,_ ,desc) package-alist)
    (unless (directory-files (package-desc-dir desc) nil "\\.elc\\'")
      (package-recompile desc))))
;; native compilation runs later in the background; log warnings to
;; *Async-native-compile-log* instead of popping up *Warnings*
(setopt native-comp-async-report-warnings-errors 'silent)
;; prefer GNU/NonGNU ELPA when a package is in more than one archive
;; (use-package's :ensure loads package.el anyway)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("gnu" . 3) ("nongnu" . 2) ("melpa" . 1)))

;;;; Shell environment
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  ;; PATH is set in .zshrc, so this needs an interactive shell
  ;; TODO update .zshrc, .profile, .zshenv files to save load time
  (setq exec-path-from-shell-warn-duration-millis 1500)
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Startup screen, bell, font
(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function #'ignore
      use-short-answers t
      confirm-kill-emacs #'y-or-n-p)
(column-number-mode 1)
(set-face-attribute 'default nil :family "Menlo" :height 180)
;; unspecified = use the frame background
(set-face-attribute 'fringe nil :background 'unspecified)

;;;; Mode line
;; hide these minor-modes
(setq mode-line-collapse-minor-modes '(auto-revert-mode guide-key-mode visual-line-mode org-indent-mode)
      mode-line-collapse-minor-modes-to "")

;;;; Cursor & scrolling
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil :background "grey93")
;; never recenter when point moves off screen; keep point's screen row when paging
(setq scroll-conservatively 1000
      scroll-preserve-screen-position t)
;; M-n/M-p scroll the view without moving point
(keymap-global-set "M-n" #'scroll-up-line)
(keymap-global-set "M-p" #'scroll-down-line)
(delete-selection-mode 1)
;; C-RET allows CUA rectangle edit. But don't remap C-x/C-c/C-v!
(setopt cua-enable-cua-keys nil)
(cua-mode 1)

;;;; Notifications
;; macOS Notification Center (brew install terminal-notifier); clicking it
;; brings Emacs forward. Falls back to the echo area.
(defun jcs:notify (title message)
  "Show MESSAGE titled TITLE as a macOS notification."
  (if-let* ((notifier (executable-find "terminal-notifier")))
      (start-process "terminal-notifier" nil notifier
                     "-title" title "-message" message "-activate" "org.gnu.Emacs")
    (message "%s: %s" title message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Keys
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
;; ...so the search prefix (occur, symbol at point, highlight) moves to C-c s
(keymap-global-set "C-c s" search-map)
(keymap-global-set "C-z" #'undo)

;;;; Mac
;; both Command and Option are Meta
(setq ns-command-modifier 'meta
      ns-alternate-modifier 'meta)

;;;; Jumping
;; C-. then a character: jump to any word starting with it (ace-jump style)
(use-package avy
  :ensure t
  :bind ("C-." . avy-goto-word-1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion & editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Completion
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

;;;; Popups
(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

;;;; Results lists
;; occur, grep and compile lists pop up without focus; step through them from
;; the code. completion lists bind the same keys while open (M-RET picks)
(keymap-global-set "M-<down>" #'next-error)
(keymap-global-set "M-<up>" #'previous-error)
;; if you do M-o into a list, moving with n/p shows each match in the source
(add-hook 'occur-mode-hook #'next-error-follow-minor-mode)
(add-hook 'compilation-mode-hook #'next-error-follow-minor-mode)

;;;; Kill ring
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

;;;; Key hints
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Backups & trash
;; keep backups and auto-saves out of working directories
(setq backup-directory-alist '((".*" . "~/.emacs.backup/"))
      auto-save-file-name-transforms '((".*" "~/.emacs.autosave/" t))
      vc-make-backup-files t
      ;; no trash-directory: nil uses the macOS Trash, so Finder's "Put Back" works
      delete-by-moving-to-trash t)
;; Emacs creates the backup dir on demand, but not the auto-save dir
(make-directory "~/.emacs.autosave/" t)

;;;; Files & buffers
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

;;;; Dired
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

;;;; Desktop
;; buffers, windows, registers and search rings; savehist covers minibuffer histories
(desktop-save-mode 1)
;; load a locked desktop only if the Emacs that locked it is gone
(setopt desktop-load-locked-desktop 'check-pid)

;;;; Remote files
;; ssh streams small files faster than the default scp
(setopt tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Text
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

;;;; Spelling
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

;;;; Markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :config
  ;; dropping a file opens it instead of inserting a link
  (setopt markdown-yank-dnd-method 'open)
  (setopt markdown-list-indent-width 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Emacs Lisp
(defun jcs:eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)) t) (current-buffer)))
(keymap-global-set "C-c e" #'jcs:eval-and-replace)

;;;; Editing code
;; M-f/M-b/M-d stop at camelCase humps; faded inline completion, TAB accepts
(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'prog-mode-hook #'completion-preview-mode)
;; follow a project's .editorconfig (indent, final newline, ...)
(editorconfig-mode 1)

;;;; Project search
;; C-x p g and xref searches use ripgrep instead of grep
(setopt xref-search-program 'ripgrep)

;;;; Tree-sitter
;; grammars: M-x treesit-install-language-grammar for typescript, tsx, python
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;;;; LSP
;; servers: npm install -g @typescript/native-preview (tsgo);
;; uv tool install basedpyright
(use-package eglot
  :hook ((typescript-ts-mode tsx-ts-mode js-ts-mode python-ts-mode) . eglot-ensure)
  :config
  ;; no event log, and don't block on connect: big repos make both slow
  (setq eglot-events-buffer-config '(:size 0)
        eglot-sync-connect nil
        eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-ts-mode) . ("tsgo" "--lsp" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("basedpyright-langserver" "--stdio")))
  (keymap-set eglot-mode-map "C-c r" #'eglot-rename)
  (keymap-set eglot-mode-map "C-c ." #'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c o" #'eglot-code-action-organize-imports)
  ;; don't offer to watch files for the server: eglot lists every project
  ;; directory and macOS spends a file descriptor per watch, so a big repo
  ;; freezes Emacs and runs out. after a branch switch, M-x eglot-reconnect
  (cl-defmethod eglot-client-capabilities :around (_server)
    (let ((caps (cl-call-next-method)))
      (plist-put (plist-get caps :workspace)
                 :didChangeWatchedFiles '(:dynamicRegistration :json-false))
      caps)))

;; faster JSON-RPC to the server; needs
;; cargo install --git https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :if (executable-find "emacs-lsp-booster")
  :after eglot
  :config
  (eglot-booster-mode 1))

;;;; Diagnostics
;; eglot reports errors through flymake; M-x flymake-show-buffer-diagnostics lists them
(use-package flymake
  :defer t
  :config
  (keymap-set flymake-mode-map "M-g e" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-g E" #'flymake-goto-prev-error))

;;;; Formatting
;; format on save; `npx' runs the project's own node_modules/.bin copy
(use-package apheleia
  :ensure t
  :demand t
  :preface
  ;; oxfmt only in projects that configure it; elsewhere apheleia's default (prettier)
  (defun jcs:apheleia-maybe-oxfmt ()
    (when (locate-dominating-file default-directory ".oxfmtrc.json")
      (setq-local apheleia-formatter 'oxfmt)))
  :hook ((typescript-ts-mode tsx-ts-mode js-ts-mode scss-mode)
         . jcs:apheleia-maybe-oxfmt)
  :config
  (setf (alist-get 'oxfmt apheleia-formatters)
        '(npx "oxfmt" "--stdin-filepath" filepath))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  (apheleia-global-mode 1))

;;;; Diff
;; unified like git/magit; -a diffs files with odd bytes as text, not "binary"
(setq diff-switches "-u -a")

;;;; Git
(use-package magit
  :ensure t
  :defer t)

;;;; Web
(use-package web-mode
  :ensure t
  :defer t)

;;;; REST
;; M-x restclient-mode in a buffer of HTTP requests; C-c C-c sends one
(use-package restclient
  :ensure t
  :defer t)

;;;; HTML export
;; used by Org for colorized source blocks in HTML export
(use-package htmlize
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start emacs server + load Org-Mode config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Server
(require 'server)
(unless (or noninteractive (server-running-p))
  (server-start))

;;;; Org
(load (expand-file-name "org-mode-init" user-emacs-directory) nil 'nomessage)

;;;; Machine-local
;; settings for this machine only (git-ignored); fine if it's missing
(load (expand-file-name "local" jcs:lisp-dir) 'noerror 'nomessage)

;;; init.el ends here
