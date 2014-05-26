
(require 'cl)

(let ((jcs:shell-path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" jcs:shell-path)
  (setq exec-path (split-string jcs:shell-path ":")))

(setq home-dir "/Users/jstautz/"
      dotemacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
      emacs-dir "/Applications/Emacs.app/Contents/"
      emacs-bin (concat emacs-dir "MacOS/Emacs")
      info-dir (concat emacs-dir "Resources/info/"))
(setq custom-file (concat dotemacs-dir "emacs-custom.el"))

(add-to-list 'load-path dotemacs-dir)
(defun jcs:decrypt-secrets ()
  (interactive)
  (require 'secrets))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'use-package)
(use-package pallet)
(require 'package)
(dolist (source '( ("gnu" . "http://elpa.gnu.org/packages/")
                   ("elpa" . "http://tromey.com/elpa/")
                   ("marmalade" . "http://marmalade-repo.org/packages/")
                   ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(add-to-list 'load-path (concat dotemacs-dir "elpa"))
;; (add-to-list 'load-path (concat dotemacs-dir "el-get/el-get"))
;; el-get, install thyself!
;; (unless (require 'el-get nil t)
;;  (url-retrieve
;;   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
;;   (lambda (s)
;;     (let ((el-get-master-branch) (el-get-install-skip-emacswiki-recipes))
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))))
;; (setq el-get-dir (concat dotemacs-dir "el-get")
;;       el-get-user-package-directory (concat dotemacs-dir "config"))
;; (require 'init-packages)
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
;; (wrap-to-fill-column-mode 1)
;; (add-hook 'text-mode-hook '(lambda() (wrap-to-fill-column-mode 1)))
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



(global-unset-key "\C-x\C-q")
(global-unset-key (kbd "<f2>"))     
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "C-z") 'undo)     
(global-set-key (kbd "<left-margin><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<left-margin><wheel-up>") 'mwheel-scroll)
(global-set-key (kbd "<right-margin><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<right-margin><wheel-up>") 'mwheel-scroll)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<C-escape>") 'top-level)  
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(add-hook 'org-mode-hook
          (lambda()
            (define-key org-mode-map (kbd "<escape>") 'keyboard-escape-quit)))
(global-set-key (kbd "M-o") 'other-window)
(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)
(global-set-key "\C-x\C-b" 'ibuffer)
;; any reason why this has to be eval'ed after load?
(eval-after-load 'dired
  '(define-key dired-mode-map "r"
     'wdired-change-to-wdired-mode))

(desktop-read)
(server-start)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'use-package)

(use-package pallet)

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

(global-unset-key "\C-x\C-q")
(global-unset-key (kbd "<f2>"))

(global-set-key (kbd "M-s") 'save-buffer)

(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "<left-margin><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<left-margin><wheel-up>") 'mwheel-scroll)
(global-set-key (kbd "<right-margin><wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<right-margin><wheel-up>") 'mwheel-scroll)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "<C-escape>") 'top-level)  
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(add-hook 'org-mode-hook
          (lambda()
            (define-key org-mode-map (kbd "<escape>") 'keyboard-escape-quit)))

(global-set-key (kbd "M-o") 'other-window)

(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)

(global-set-key "\C-x\C-b" 'ibuffer)

;; any reason why this has to be eval'ed after load?
(eval-after-load 'dired
  '(define-key dired-mode-map "r"
     'wdired-change-to-wdired-mode))

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
;; (wrap-to-fill-column-mode 1)
;; (add-hook 'text-mode-hook '(lambda() (wrap-to-fill-column-mode 1)))
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
;; (add-to-list 'load-path (concat dotemacs-dir "el-get/el-get"))

;; el-get, install thyself!
;; (unless (require 'el-get nil t)
;;  (url-retrieve
;;   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
;;   (lambda (s)
;;     (let ((el-get-master-branch) (el-get-install-skip-emacswiki-recipes))
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))))

;; (setq el-get-dir (concat dotemacs-dir "el-get")
;;       el-get-user-package-directory (concat dotemacs-dir "config"))

;; (require 'init-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A random smattering of custom/helper functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;-----------------------------------------------------------------------------
;; Eval and replace -- replace current sexp with its value
;;-----------------------------------------------------------------------------

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))

(global-set-key (kbd "C-c e") 'eval-and-replace)


;;-----------------------------------------------------------------------------
;; i-search with initial contents.
;; original src: http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
;;-----------------------------------------------------------------------------
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(global-set-key (kbd "C-c C-s") 'isearch-forward-at-point)


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


;;-----------------------------------------------------------------------------
;; Make zap-to-char act more like "zap-up-to-char"
;; Thanks to Eric Himmelreich
;; http://rawsyntax.com/blog/learn-emacs-use-defadvice-modify-functions/
;;-----------------------------------------------------------------------------
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))


;;-----------------------------------------------------------------------------
;; Window-splitting and moving functions
;; Thanks to Ignacio Paz Posse
;; http://ignaciopp.wordpress.com/2009/05/23/emacs-manage-windows-split/
;;-----------------------------------------------------------------------------

;; Instead of giving me two identical buffers when I split the window, give me the previous buffer.
;; From http://www.reddit.com/r/emacs/comments/25v0eo/you_emacs_tips_and_tricks/chldury
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


;; Switch (zoom) between split window config and a single window
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

;; Window shifting. C-x-o lets us go forward a window (or several).
;; This one lets us go back one or more windows. From Glickstein.
(defun other-window-backward (&optional n)
  "Select previous Nth window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key [prior] 'other-window)
(global-set-key [next] 'other-window-backward)
(global-set-key [(control tab)] 'other-window)
(global-set-key [(shift control tab)] 'other-window-backward)

;; Also (stolen from someone else's dot file) this is used to shrink/expand windows without using the mouse.
(define-key global-map (kbd "C-M-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "C-M-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "C-M-<up>") 'enlarge-window)
(define-key global-map (kbd "C-M-<down>") 'shrink-window)


;;-----------------------------------------------------------------------------
;; Open in Marked.app -- Stolen from https://github.com/mattsears/emacs
;;-----------------------------------------------------------------------------
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
           (shell-quote-argument (buffer-file-name)))))

(global-set-key "\C-cm" 'markdown-preview-file)


;;-----------------------------------------------------------------------------
;; Unfill paragraph -- From https://raw.github.com/qdot/conf_emacs/master/emacs_conf.org
;;-----------------------------------------------------------------------------
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;-----------------------------------------------------------------------------
;; Re-open buffer as root -- Thank to @christopherdone: http://t.co/KiAWcJoo
;;-----------------------------------------------------------------------------
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
;; auto-recompile elisp -- Thanks to Adolfo Benedetti and Xah Lee
;;-----------------------------------------------------------------------------
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)


;;-----------------------------------------------------------------------------
;; A smarter find-tag that automagically reruns etags when it can't find a
;; requested item and then makes a new try to locate it.
;; by Jonas.Jarnestrom<at>ki.ericsson.se
;; Fri Mar 15 09:52:14 2002
;;-----------------------------------------------------------------------------
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

;; While we're at it, modify pop-tag-mark to stay centered on cursor
(defadvice pop-tag-mark (after my-pop-tag-mark-advice activate)
  "After popping back to where find-tag was invoked,
   center screen on cursor"
  (let ((current-prefix-arg '(4)))
  (call-interactively 'recenter-top-bottom)))


;;-----------------------------------------------------------------------------
;; Renaming and deleting files should be easier to do. 
;; C-x C-r renames current file
;; C-x C-k deletes current file (since C-x k kills the buffer)
;;-----------------------------------------------------------------------------

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


;;-----------------------------------------------------------------------------
;; Make beginning-of-buffer and end-of-buffer work nicely in dired
;; Thanks to @magnars
;;-----------------------------------------------------------------------------

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(require 'cl)

(let ((jcs:shell-path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" jcs:shell-path)
  (setq exec-path (split-string jcs:shell-path ":")))

(setq home-dir "/Users/jstautz/"
      dotemacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
      emacs-dir "/Applications/Emacs.app/Contents/"
      emacs-bin (concat emacs-dir "MacOS/Emacs")
      info-dir (concat emacs-dir "Resources/info/"))

(setq custom-file (concat dotemacs-dir "emacs-custom.el"))

(add-to-list 'load-path dotemacs-dir)

(defun jcs:decrypt-secrets ()
  (interactive)
  (require 'secrets))

(desktop-read)

(server-start)
