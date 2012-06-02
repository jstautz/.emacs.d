;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Editing options, UI config, custom keybindings, 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;-----------------------------------------------------------------------------
;; System / Editing Prefs
;;-----------------------------------------------------------------------------

;; Put backups & autosaves in their place (not in current dir)
(defvar backup-dir (concat home-dir ".emacs.backup/"))
(defvar autosave-dir (concat home-dir ".emacs.autosave/"))
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; When deleting files, move them to Trash
(setq delete-by-moving-to-trash t)
(setq trash-directory (concat home-dir ".Trash"))

;; Revert any buffer when file on disk changes 
(setq global-auto-revert-mode 1)             

;; Make mouse/keyboard/EOL/clipboard work sanely on OS X
(setq mac-emulate-three-button-mouse t)
(setq ns-alternate-modifier (quote meta))
(setq ns-command-modifier (quote meta))
(setq eol-mnemonic-mac "(Mac)")
(setq x-select-enable-clipboard t)

;; M-x locate use OS X's Spotlight
(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))

;; If I drag n' drop a file onto Emacs, visit the file (instead of append to buffer)
(define-key global-map [ns-drag-file] 'ns-find-file) 

;; Tabs insert 4 spaces, sentences have one space.
;;   TODO may want to look at using smart-tabs-mode
;;   See http://www.emacswiki.org/emacs/SmartTabs and
;;   https://github.com/jcsalomon/smarttabs
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
(auto-fill-mode 1)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interface Tweaks
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make display feel faster
(setq redisplay-dont-pause t)

;; No splash screen, no beeping, no toolbar/scrollbar
(setq inhibit-splash-screen 1)               
(setq visible-bell 1)                        
(setq ring-bell-function (lambda ()))
(tool-bar-mode 0)
(scroll-bar-mode nil)

;; TODO -- want to adjust these?
(setq 
  scroll-margin 0
  scroll-conservatively 0
  scroll-up-aggressively nil
  scroll-down-aggressively nil
  scroll-preserve-screen-position nil)

(mouse-avoidance-mode 'jump)

;; ignore case when completing, including filenames
(setq completion-ignore-case t           
  read-file-name-completion-ignore-case t)

;; Where's my cursor?
(setq blink-cursor-mode t)

;; Don't make me type "y-e-s"
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't let me kill emacs accidentally
(setq confirm-kill-emacs 'y-or-n-p)

;; Show line and column #
(line-number-mode 1)                         
(column-number-mode 1)

;; Keep emacs server from opening new frame each time
(setq ns-pop-up-frames nil)

;; Raise emacs function
(defun ns-raise-emacs ()
  (ns-do-applescript "tell application \"Emacs\" to activate"))
(add-hook 'server-visit-hook 'ns-raise-emacs)

;; Let me narrow to region -- I use this a bunch
(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Keybindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rebind M-s to save-buffer -- I never center lines
(global-set-key (kbd "M-s") 'save-buffer)
(add-hook 'text-mode-hook
 (lambda ()
 (define-key text-mode-map (kbd "M-s") 'save-buffer)
 )
)

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
(define-key global-map (kbd "RET") 'newline-and-indent)

;; wdired toggle -- for rewriting filenames. hit 'r' in dired mode
(eval-after-load 'dired
  '(define-key dired-mode-map "r"
     'wdired-change-to-wdired-mode))





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

)


(provide 'init-customizations)
