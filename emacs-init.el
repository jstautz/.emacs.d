
(require 'package)
(dolist (source '( ("gnu" . "http://elpa.gnu.org/packages/")
                   ("elpa" . "http://tromey.com/elpa/")
                   ("marmalade" . "http://marmalade-repo.org/packages/")
                   ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)

(require 'use-package)

(use-package cl)

(let ((jcs:shell-path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" jcs:shell-path)
  (setq exec-path (split-string jcs:shell-path ":")))

(setq home-dir "/Users/jstautz/"
      dotemacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
      emacs-dir "/Applications/Emacs.app/Contents/"
      emacs-bin (concat emacs-dir "MacOS/Emacs")
      info-dir (concat emacs-dir "Resources/info/"))

(add-to-list 'load-path dotemacs-dir)

(setq custom-file (concat dotemacs-dir "emacs-custom.el"))

(setq gc-cons-threshold 20000000)

(defun jcs:decrypt-secrets ()
  (interactive)
  (require 'secrets))

(tool-bar-mode -1)

(set-face-background 'fringe (face-background 'default))
(set-face-foreground 'fringe (face-background 'default))

(scroll-bar-mode -1)

(setq inhibit-splash-screen 1)               
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(setq ring-bell-function (lambda ()))
(setq visible-bell 1)

(fset 'yes-or-no-p 'y-or-n-p)

(defun toggle-fullscreen ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
       nil 'fullscreen
       (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; (toggle-fullscreen)

(global-unset-key "\C-x\C-c")
(setq confirm-kill-emacs 'y-or-n-p)

(setq custom-safe-themes t)

(line-number-mode 1)                         
(column-number-mode 1)

(use-package smart-mode-line
  :init
  (progn

(setq sml/theme 'respectful
      sml/name-width 40
      sml/mode-width 40)

(setq sml/replacer-regexp-list
  (quote
   (("^~/org/" ":Org:")
    ("^~/\\.emacs\\.d/" ":ED:")
    ("^~/[Gg]it/" ":Git:")
    ("^~/Dropbox/Writing/01-composting/" ":Compost:")
    ("^~/Dropbox/Writing/02-draft_in_progress/" ":Drafts:")
    ("^~/Dropbox/Writing/03-revision_in_progress/" ":Revs:")
    ("^~/Dropbox/Writing/04-submitted/" ":Submitted:")
    ("^~/Dropbox/Writing/05-published/" ":Published:")
    ("^~/Dropbox/Writing/06-cold_storage/" ":ColdStore:")
    ("^~/Dropbox/Writing/" ":Writing:")
    ("^~/Dropbox/finance/taxes 2013/" ":Tax2013:")
    ("^~/Dropbox/finance/taxes 2014/" ":Tax2014:")
    ("^~/Dropbox/" ":DB:")
    ("^~/Documents/Writing/01-composting/" ":Compost:")
    ("^~/Documents/Writing/02-draft_in_progress/" ":Drafts:")
    ("^~/Documents/Writing/03-revision_in_progress/" ":Revs:")
    ("^~/Documents/Writing/04-submitted/" ":Submitted:")
    ("^~/Documents/Writing/05-published/" ":Published:")
    ("^~/Documents/Writing/06-cold_storage/" ":ColdStore:")
    ("^~/Documents/Writing/" ":Writing:")
    ("^~/Documents/" ":Docs:")
    ("^~/Documents/Work/" ":Work:")
    ("^~/dev" ":dev:")
    ("^~/Sites" ":www:")
    ("^~/Downloads/" ":DLs:"))))

(sml/setup)))

(use-package diminish)

(setq redisplay-dont-pause t)

(defvar backup-dir (concat home-dir ".emacs.backup/"))
(defvar autosave-dir (concat home-dir ".emacs.autosave/"))
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

(setq vc-make-backup-files t)

(setq delete-by-moving-to-trash t)
(setq trash-directory (concat home-dir ".Trash/"))

(global-unset-key "\C-x\C-q")
(global-unset-key (kbd "<f2>"))

(global-set-key (kbd "M-s") 'save-buffer)

(global-set-key (kbd "C-z") 'undo)

(setq ns-alternate-modifier (quote meta))
(setq ns-command-modifier (quote meta))

(setq mac-emulate-three-button-mouse t)

(setq blink-cursor-mode t)

(global-hl-line-mode 1)
(set-face-background 'hl-line "grey93")

(setq scroll-conservatively 1000)

(setq scroll-margin 0)

(setq scroll-up-aggressively nil
      scroll-down-aggressively nil
      scroll-preserve-screen-position t)

(global-set-key (kbd "<left-margin><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<left-margin><wheel-up>") 'mwheel-scroll)
(global-set-key (kbd "<right-margin><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<right-margin><wheel-up>") 'mwheel-scroll)
(global-set-key (kbd "<mode-line><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<mode-line><wheel-up>") 'mwheel-scroll)

(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)

(transient-mark-mode t)
(delete-selection-mode t)

(setq cua-enable-cua-keys nil)               
(cua-mode t)

(use-package multiple-cursors
:bind (("C-+" . mc/mark-next-like-this)
       ("C--" . mc/mark-previous-like-this)
       ("C-*" . mc/mark-all-like-this)

("C-x a l" . mc/edit-lines)
("C-x a e" . mc/edit-ends-of-lines)
("C-x a a" . mc/edit-beginnings-of-lines)))

(use-package expand-region
:bind (("C-=" . er/expand-region)))

;; auto-fill options 
(setq fill-column 120)
(setq default-fill-column 120)

(setq eol-mnemonic-mac "(Mac)")

;;(auto-fill-mode 1)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(require 'wrap-to-fill)
(visual-line-mode 1)
(wrap-to-fill-column-mode 1)
(add-hook 'text-mode-hook '(lambda() (wrap-to-fill-column-mode 1)))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

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

(setq bookmark-default-file (concat dotemacs-dir "bookmarks"))

(winner-mode 1)

(global-set-key (kbd "M-o") 'other-window)

(use-package ace-window
             :bind ("M-p" . ace-window))

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

(defun other-window-backward (&optional n)
  "Select previous Nth window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key [prior] 'other-window)
(global-set-key [next] 'other-window-backward)
(global-set-key [(control tab)] 'other-window)
(global-set-key [(shift control tab)] 'other-window-backward)

(define-key global-map (kbd "C-M-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "C-M-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "C-M-<up>") 'enlarge-window)
(define-key global-map (kbd "C-M-<down>") 'shrink-window)

(global-set-key "\C-x\C-b" 'ibuffer)

(eval-after-load 'dired
  '(define-key dired-mode-map "r"
     'wdired-change-to-wdired-mode))

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

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq global-auto-revert-mode 1)

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

(define-key global-map [ns-drag-file] 'ns-find-file)

(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))

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

;;-----------------------------------------------------------------------------
;; Nice functions to change copy-kill region to copy or kill current line
;; if no active region.
;; Stolen from http://xahlee.org/emacs/emacs_copy_cut_current_line.html
;;-----------------------------------------------------------------------------
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line copied to kill-ring.")
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(use-package ace-jump-mode
             :bind ("C-." . ace-jump-mode))

(setq x-select-enable-clipboard t)

(use-package browse-kill-ring
             :defer t
             :init
             (progn
               (autoload 'browse-kill-ring-default-keybindings "browse-kill-ring")
               (browse-kill-ring-default-keybindings)))

(use-package kill-ring-search
             :bind ("\M-\C-y" . kill-ring-search))

(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
    "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
    The CHAR is replaced and the point is put before CHAR."
    (insert char)
    (forward-char -1))

(use-package easy-kill
             :defer t
             :init
             (global-set-key [remap kill-ring-save] 'easy-kill))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line copied to kill-ring.")
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(setq ns-pop-up-frames nil)

(ido-mode t)
(setq ido-enable-flex-matching t
      ido-enable-tramp-completion nil
      ;; since we're using ido-ubiquitous, we don't need this on
      ido-everywhere nil
      ido-is-tramp-root nil
      ido-max-prospects 20
      ;; don't save tramp work directories -- this caused stalls for me in the past
      ido-record-ftp-work-directories nil
      ido-show-dot-for-dired t
      ;; be able to re-visit recently closed buffers
      ido-use-virtual-buffers t
      ;; No automatic searching if no matches found
      ido-auto-merge-work-directories-length -1
      ido-use-faces t
      ;; for find-file-at-point
      ido-use-filename-at-point 'guess)
;; use recentf for visiting recent buffers
(recentf-mode t)
;; don't need this function disabled (it is by default)
(put 'ido-exit-minibuffer 'disabled nil)

;; ignore case when completing, including filenames
  (setq completion-ignore-case t           
    read-file-name-completion-ignore-case t)

(use-package ido-ubiquitous
             :init (ido-ubiquitous))

(use-package ido-vertical-mode
             :init
             (progn (ido-vertical-mode 1)
                    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))

(defun jcs:smex-init ()
  (interactive)
  (condition-case description
      (progn
        (smex-initialize)
        (global-set-key (kbd "M-x") 'smex)
        (global-set-key (kbd "M-X") 'smex-major-mode-commands)
        (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
        (global-set-key (kbd "C-c C-m") 'smex)
        (global-set-key (kbd "C-x C-m") 'smex)
        (smex))
    (error (execute-extended-command))))
(global-set-key (kbd "M-x") 'jcs:smex-init)

(defun guide-key/jcs-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x"))
(add-hook 'org-mode-hook 'guide-key/jcs-hook-function-for-org-mode)

(setq guide-key/idle-delay 1)

(setq guide-key/popup-window-position 'bottom)

(guide-key-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq sentence-end-double-space nil)

(setq ispell-program-name "aspell")

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(put 'narrow-to-region 'disabled nil)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package markdown-mode
           :mode "\\.\\(md\\|mdown\\|markdown\\)\\'")

(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
           (shell-quote-argument (buffer-file-name)))))

(global-set-key "\C-cm" 'markdown-preview-file)

(use-package fountain-mode)

(use-package wordsmith-mode)

(setq tramp-default-method "ssh")

(global-set-key (kbd "RET") 'newline-and-indent)

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

(setq tags-revert-without-query t)

(defadvice pop-tag-mark (after my-pop-tag-mark-advice activate)
  "After popping back to where find-tag was invoked,
   center screen on cursor"
  (let ((current-prefix-arg '(4)))
  (call-interactively 'recenter-top-bottom)))

(setq diff-switches "-a -c")

(use-package magit
             :diminish magit-auto-revert-mode)

(defun eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (prin1 (eval (read (current-kill 0)))
           (current-buffer)))
  
  (global-set-key (kbd "C-c e") 'eval-and-replace)

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

(global-set-key (kbd "<C-escape>") 'top-level)  
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(add-hook 'org-mode-hook
          (lambda()
            (define-key org-mode-map (kbd "<escape>") 'keyboard-escape-quit)))

(use-package virtualenvwrapper)

(use-package jedi)

(use-package web-mode)

(use-package rainbow-mode)

(use-package skewer-mode)

(use-package js2-mode)

(use-package js2-refactor)

(use-package json-mode)

;;-----------------------------------------------------------------------------
;; jcs:getcals -- Sync my Google Calendars to emacs diary
;;-----------------------------------------------------------------------------
(require 'icalendar)

(defun getcal (url)
  "Download ics file and add to diary"
  (let ((tmpfile (url-file-local-copy url)))
    (icalendar-import-file tmpfile "~/org/calendar.diary" t)
    (kill-buffer (car (last (split-string tmpfile "/"))))
    )
  )

;; Grab google calendars from secrets.el.gpg
(defun jcs:getcals ()
  (interactive)
  (if (not (boundp 'google-calendars))
      (jcs:decrypt-secrets))
    (find-file "~/org/calendar.diary")
    (flush-lines "^[& ]")
    (dolist (url google-calendars) (getcal url))
    (kill-buffer "calendar.diary"))


;;-----------------------------------------------------------------------------
;; jcs:clock functions -- Functions to clock into/out of  a particular item in
;; projects.org (OR create a new item and clock into it)
;;-----------------------------------------------------------------------------
 (defun jcs:clock-in-to-string (theString &optional theCategory)
  "Clock into a particular item in ~/org/projects.org file. Takes optional Category param."
  (interactive)
  (save-excursion
    (let (filepath filename mybuffer)
      (setq filepath "/Users/jstautz/org/projects.org"
            filename (file-name-nondirectory filepath)
            mybuffer (find-file filepath))
      (goto-char (point-min))
      (widen) 
      ;; if no category defined, try to find string in file and clock in
      (if (eq theCategory nil)
          (if (search-forward theString nil t)
              (org-clock-in)
            ;; if not found in buffer, insert new item at end and clock into it
            (goto-char (point-max))
            (insert (concat "*** " theString))
            (goto-char (point-max))
            (org-clock-in))
        ;; thecategory is non-nil, so this is a new item w/ category
        (goto-char (point-max))
        (insert (concat "*** " theString "\n  :PROPERTIES:\n  :CATEGORY: " theCategory "\n  :END:\n"))
        (goto-char (point-max))
        (org-clock-in)))))

(defun jcs:clock-out (&optional theString theCategory)
  (org-clock-out))

(desktop-read)

(server-start)

(load-file "~/.emacs.d/config/init-org-mode.el")

(load-file "~/.emacs.d/.cask/24.3.92.1/elpa/org-plus-contrib-20140630/org-habit.el")
