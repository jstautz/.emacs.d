(require 'cl)

(let ((jcs:shell-path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" jcs:shell-path)
  (setq exec-path (split-string jcs:shell-path ":")))

(setq home-dir "/Users/jstautz/"
      dotemacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
      emacs-dir "/Applications/Emacs.app/Contents/"
      emacs-bin (concat emacs-dir "MacOS/Emacs")
      info-dir (concat emacs-dir "Resources/info/"))
(setq custom-file (concat dotemacs-dir "emacs-init.el"))

(add-to-list 'load-path dotemacs-dir)
(defun jcs:decrypt-secrets ()
  (interactive)
  (require 'secrets))

(require 'package)
(dolist (source '( ("gnu" . "http://elpa.gnu.org/packages/")
                   ("elpa" . "http://tromey.com/elpa/")
                   ("marmalade" . "http://marmalade-repo.org/packages/")
                   ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(add-to-list 'load-path (concat dotemacs-dir "elpa"))
(add-to-list 'load-path (concat dotemacs-dir "el-get/el-get"))
;; el-get, install thyself!
(unless (require 'el-get nil t)
 (url-retrieve
  "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
  (lambda (s)
    (let ((el-get-master-branch) (el-get-install-skip-emacswiki-recipes))
      (goto-char (point-max))
      (eval-print-last-sexp)))))
(setq el-get-dir (concat dotemacs-dir "el-get")
      el-get-user-package-directory (concat dotemacs-dir "config"))
(require 'init-packages)
(tool-bar-mode -1)
(custom-set-faces '(fringe ((t (:background"white")))))
(scroll-bar-mode -1)

(setq inhibit-splash-screen 1)               
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(setq ring-bell-function (lambda ()))
(setq visible-bell 1)                        

(fset 'yes-or-no-p 'y-or-n-p)

(setq blink-cursor-mode t)
(global-hl-line-mode 1)
(set-face-background 'hl-line "grey93")

(setq scroll-conservatively 1000)
(setq scroll-margin 0)
(setq scroll-up-aggressively nil
      scroll-down-aggressively nil
      scroll-preserve-screen-position t)

(setq ns-alternate-modifier (quote meta))
(setq ns-command-modifier (quote meta))
(setq eol-mnemonic-mac "(Mac)")
(setq x-select-enable-clipboard t)
(setq mac-emulate-three-button-mouse t)

(define-key global-map [ns-drag-file] 'ns-find-file) 

(line-number-mode 1)                         
(column-number-mode 1)

(global-unset-key "\C-x\C-c")
(setq confirm-kill-emacs 'y-or-n-p)

(setq ns-pop-up-frames nil)

(setq redisplay-dont-pause t)

;;-----------------------------------------------------------------------------
;; System / Editing Prefs
;;-----------------------------------------------------------------------------

;; Set gabage collection threshold higher, as per https://github.com/lewang/flx
(setq gc-cons-threshold 20000000)

;; Put backups & autosaves in their place (not in current dir)
(defvar backup-dir (concat home-dir ".emacs.backup/"))
(defvar autosave-dir (concat home-dir ".emacs.autosave/"))
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; When deleting files, move them to Trash
(setq delete-by-moving-to-trash t)
(setq trash-directory (concat home-dir ".Trash/"))

;; Refresh any buffer when file on disk changes
(setq global-auto-revert-mode 1)

;; ...Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; ...Also rebuild Tags files, and be quiet about it
(setq tags-revert-without-query t)

;; M-x locate uses OS X's Spotlight
(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))


;; Tabs insert 4 spaces, sentences have one space (as God intended).
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)

;; selection options + rectangle selection w/ CUA
(transient-mark-mode t)
(delete-selection-mode t)
(setq cua-enable-cua-keys nil)               
(cua-mode t)

;; auto-fill options 
(setq fill-column 120)
(setq default-fill-column 120)

;; Testing wrap-to-fill options instead of auto-fill
;; note: I think wrap-to-fill is only included in nxhtml-mode,
;;       so we need packages installed before this works.
;; 
;;(auto-fill-mode 1)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(wrap-to-fill-column-mode 1)
(add-hook 'text-mode-hook '(lambda() (wrap-to-fill-column-mode 1)))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Desktop saving options
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

;; Quick ways to restore desktop/windows
(winner-mode 1)

;; Bookmark options
(setq bookmark-default-file (concat dotemacs-dir "bookmarks"))

;; spellcheck options
(setq ispell-program-name "aspell")

;; Tramp defaults
(setq tramp-default-method "ssh")

;; Make sure diff works with .org files and others
(setq diff-switches "-a -c")

;; Let me upcase/downcase regions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; ignore case when completing, including filenames
(setq completion-ignore-case t           
  read-file-name-completion-ignore-case t)

;; Let me narrow to region -- I use this a bunch
(put 'narrow-to-region 'disabled nil)



;; Up yours, read-only toggle!
(global-unset-key "\C-x\C-q")

;; Rebind M-s to save-buffer -- I never center lines
(global-set-key (kbd "M-s") 'save-buffer)

;; Don't think I need this, if I set it globally?
;; (add-hook 'text-mode-hook
;;  (lambda ()
;;  (define-key text-mode-map (kbd "M-s") 'save-buffer)
;;  )
;; )

;; Rebind M-o to other-window. C-x o is too clunky.
(global-set-key (kbd "M-o") 'other-window)

;; Bind C-esc to 'top-level for exiting from debugger, etc.
(global-set-key (kbd "<C-escape>") 'top-level)  
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(add-hook 'org-mode-hook
          (lambda()
            (define-key org-mode-map (kbd "<escape>") 'keyboard-escape-quit)))

;; I like Undo here.
(global-set-key (kbd "C-z") 'undo)     

;; I also like iBuffer here
(global-set-key "\C-x\C-b" 'ibuffer)

;; I hit F2 accidentally way too often
(global-unset-key (kbd "<f2>"))     

;; Scroll buffer
(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)

;; Return should preserve indentation (will override in modes where I don't like this)
(global-set-key (kbd "RET") 'newline-and-indent)

;; wdired toggle -- for rewriting filenames. hit 'r' in dired mode
;; any reason why this has to be eval'ed after load?
(eval-after-load 'dired
  '(define-key dired-mode-map "r"
     'wdired-change-to-wdired-mode))


;; If I scroll with my mouse wheel on the fringe/margin, handle this:
(global-set-key (kbd "<left-margin><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<left-margin><wheel-up>") 'mwheel-scroll)
(global-set-key (kbd "<right-margin><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<right-margin><wheel-up>") 'mwheel-scroll)

(require 'init-custom-functions)
(desktop-read)
(server-start)

(tool-bar-mode -1)

(custom-set-faces '(fringe ((t (:background"white")))))

(scroll-bar-mode -1)

(setq inhibit-splash-screen 1)               
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(setq ring-bell-function (lambda ()))
(setq visible-bell 1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq blink-cursor-mode t)

(global-hl-line-mode 1)
(set-face-background 'hl-line "grey93")

(setq scroll-conservatively 1000)

(setq scroll-margin 0)

(setq scroll-up-aggressively nil
      scroll-down-aggressively nil
      scroll-preserve-screen-position t)

(setq ns-alternate-modifier (quote meta))
(setq ns-command-modifier (quote meta))

(setq eol-mnemonic-mac "(Mac)")

(setq x-select-enable-clipboard t)

(setq mac-emulate-three-button-mouse t)

(define-key global-map [ns-drag-file] 'ns-find-file)

(line-number-mode 1)                         
(column-number-mode 1)

(global-unset-key "\C-x\C-c")
(setq confirm-kill-emacs 'y-or-n-p)

(setq ns-pop-up-frames nil)

(setq redisplay-dont-pause t)

;; Up yours, read-only toggle!
(global-unset-key "\C-x\C-q")

;; Rebind M-s to save-buffer -- I never center lines
(global-set-key (kbd "M-s") 'save-buffer)

;; Don't think I need this, if I set it globally?
;; (add-hook 'text-mode-hook
;;  (lambda ()
;;  (define-key text-mode-map (kbd "M-s") 'save-buffer)
;;  )
;; )

;; Rebind M-o to other-window. C-x o is too clunky.
(global-set-key (kbd "M-o") 'other-window)

;; Bind C-esc to 'top-level for exiting from debugger, etc.
(global-set-key (kbd "<C-escape>") 'top-level)  
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(add-hook 'org-mode-hook
          (lambda()
            (define-key org-mode-map (kbd "<escape>") 'keyboard-escape-quit)))

;; I like Undo here.
(global-set-key (kbd "C-z") 'undo)     

;; I also like iBuffer here
(global-set-key "\C-x\C-b" 'ibuffer)

;; I hit F2 accidentally way too often
(global-unset-key (kbd "<f2>"))     

;; Scroll buffer
(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)

;; Return should preserve indentation (will override in modes where I don't like this)
(global-set-key (kbd "RET") 'newline-and-indent)

;; wdired toggle -- for rewriting filenames. hit 'r' in dired mode
;; any reason why this has to be eval'ed after load?
(eval-after-load 'dired
  '(define-key dired-mode-map "r"
     'wdired-change-to-wdired-mode))


;; If I scroll with my mouse wheel on the fringe/margin, handle this:
(global-set-key (kbd "<left-margin><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<left-margin><wheel-up>") 'mwheel-scroll)
(global-set-key (kbd "<right-margin><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<right-margin><wheel-up>") 'mwheel-scroll)

;;-----------------------------------------------------------------------------
;; System / Editing Prefs
;;-----------------------------------------------------------------------------

;; Set gabage collection threshold higher, as per https://github.com/lewang/flx
(setq gc-cons-threshold 20000000)

;; Put backups & autosaves in their place (not in current dir)
(defvar backup-dir (concat home-dir ".emacs.backup/"))
(defvar autosave-dir (concat home-dir ".emacs.autosave/"))
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; When deleting files, move them to Trash
(setq delete-by-moving-to-trash t)
(setq trash-directory (concat home-dir ".Trash/"))

;; Refresh any buffer when file on disk changes
(setq global-auto-revert-mode 1)

;; ...Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; ...Also rebuild Tags files, and be quiet about it
(setq tags-revert-without-query t)

;; M-x locate uses OS X's Spotlight
(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))


;; Tabs insert 4 spaces, sentences have one space (as God intended).
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)

;; selection options + rectangle selection w/ CUA
(transient-mark-mode t)
(delete-selection-mode t)
(setq cua-enable-cua-keys nil)               
(cua-mode t)

;; auto-fill options 
(setq fill-column 120)
(setq default-fill-column 120)

;; Testing wrap-to-fill options instead of auto-fill
;; note: I think wrap-to-fill is only included in nxhtml-mode,
;;       so we need packages installed before this works.
;; 
;;(auto-fill-mode 1)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(wrap-to-fill-column-mode 1)
(add-hook 'text-mode-hook '(lambda() (wrap-to-fill-column-mode 1)))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Desktop saving options
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

;; Quick ways to restore desktop/windows
(winner-mode 1)

;; Bookmark options
(setq bookmark-default-file (concat dotemacs-dir "bookmarks"))

;; spellcheck options
(setq ispell-program-name "aspell")

;; Tramp defaults
(setq tramp-default-method "ssh")

;; Make sure diff works with .org files and others
(setq diff-switches "-a -c")

;; Let me upcase/downcase regions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; ignore case when completing, including filenames
(setq completion-ignore-case t           
  read-file-name-completion-ignore-case t)

;; Let me narrow to region -- I use this a bunch
(put 'narrow-to-region 'disabled nil)

(require 'package)
(dolist (source '( ("gnu" . "http://elpa.gnu.org/packages/")
                   ("elpa" . "http://tromey.com/elpa/")
                   ("marmalade" . "http://marmalade-repo.org/packages/")
                   ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(add-to-list 'load-path (concat dotemacs-dir "elpa"))
(add-to-list 'load-path (concat dotemacs-dir "el-get/el-get"))

;; el-get, install thyself!
(unless (require 'el-get nil t)
 (url-retrieve
  "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
  (lambda (s)
    (let ((el-get-master-branch) (el-get-install-skip-emacswiki-recipes))
      (goto-char (point-max))
      (eval-print-last-sexp)))))

(setq el-get-dir (concat dotemacs-dir "el-get")
      el-get-user-package-directory (concat dotemacs-dir "config"))

(require 'init-packages)

(require 'init-custom-functions)

(require 'cl)

(let ((jcs:shell-path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" jcs:shell-path)
  (setq exec-path (split-string jcs:shell-path ":")))

(setq home-dir "/Users/jstautz/"
      dotemacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
      emacs-dir "/Applications/Emacs.app/Contents/"
      emacs-bin (concat emacs-dir "MacOS/Emacs")
      info-dir (concat emacs-dir "Resources/info/"))

(setq custom-file (concat dotemacs-dir "emacs-init.el"))

(add-to-list 'load-path dotemacs-dir)

(defun jcs:decrypt-secrets ()
  (interactive)
  (require 'secrets))

(desktop-read)

(server-start)
