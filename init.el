;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Index 
;;-----------------------------------------------------------------------------
;; - System / Editing Prefs
;; - Interface
;; - Keybindings
;; - Major Modes & Customizations
;; - Org-mode Customizations
;; - Custom functions, aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Preflight
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(defvar *emacs-load-start* (current-time))

;; Path/install configs
(setq home-dir "/Users/jstautz/")
(setq dotemacs-dir (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'load-path dotemacs-dir)
(setq emacs-dir "/Applications/Emacs.app/Contents/")
(setq emacs-bin (concat emacs-dir "MacOS/Emacs"))
(setq info-dir (concat emacs-dir "Resources/info/"))

;; Let's get this party started.
(server-start)

(setq toggle-debug-on-quit 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; System / Editing Prefs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Put backups & autosaves in their place
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
(setq ;; scrolling
  scroll-margin 0                        ;; do smooth scrolling, ...
  scroll-conservatively 0           ;; ... the defaults ...
  scroll-up-aggressively nil               ;; ... are very ...
  scroll-down-aggressively nil             ;; ... annoying
  scroll-preserve-screen-position nil)     ;; preserve screen pos with C-v/M-v

(mouse-avoidance-mode 'jump)

(setq completion-ignore-case t           ;; ignore case when completing...
  read-file-name-completion-ignore-case t) ;; ...filenames too

(setq initial-scratch-message
  ";; scratch buffer created -- happy hacking\n")


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Major + Minor Modes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up Emacs package repositories
(require 'package)
(dolist (source '( ("gnu" . "http://elpa.gnu.org/packages/")
                   ("elpa" . "http://tromey.com/elpa/")
                   ("technomancy" . "http://repo.technomancy.us/emacs/")
                   ("marmalade" . "http://marmalade-repo.org/packages/")
                   ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(add-to-list 'load-path (concat dotemacs-dir "elpa"))

;; el-get, install thyself
(add-to-list 'load-path (concat dotemacs-dir "el-get/el-get"))
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

(setq el-get-dir (concat dotemacs-dir "el-get/"))

;; Recipes for packages
(setq el-get-sources  
      '(
        (:name android
               :description "Included with Android SDK - connect Emacs to emulator"
               :type http
               :url "https://raw.github.com/evnm/emacs/master/vendor/android.el"
               :compile ("android.el")
               :features android)
        
        (:name android-mode
               :description "Minor mode for Android development"
               :type elpa)
        
        (:name browse-kill-ring
               :description "View kill-ring with M-y"
               :type elpa
               :post-init (lambda()
                            (browse-kill-ring-default-keybindings)))
        
        (:name css-mode :type elpa)
        
        (:name edb
               :description "Emacs database mode"
               :type git
               :url "https://github.com/emacsmirror/edb"
               :build `(,(concat "./configure --prefix=" el-get-dir "edb/ --infodir=" info-dir
                                 " --with-sitelisp=" el-get-dir "edb/lisp/")
                        ,(concat "make EMACS=" emacs-bin)
                        "make install")
               :load-path "lisp"
               :features database)

        (:name elip
               :description "Emacs Learning mode (flashcards)"
               :type http
               :url "https://raw.github.com/emacsmirror/elip/master/source/elip.el"
               :depends edb
               :features elip)
        
        (:name ido
               :description "Interactively Do Things. Fuzzy match buffers/files/etc."
               :type http
               :url "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/ido.el"
               :compile ("ido.el")
               :post-init (lambda()
                            (ido-mode t)
                            (setq ido-enable-flex-matching t)
                            (setq ido-enable-tramp-completion nil)
                            (setq ido-everywhere t)
                            (setq ido-is-tramp-root nil)
                            (setq ido-max-prospects 10)
                            (setq ido-record-ftp-work-directories nil)
                            (setq ido-show-dot-for-dired t)
                            (put 'ido-exit-minibuffer 'disabled nil)))
        
        (:name js2-mode-mooz
               :description "Improved JavaScript mode -- forked from Steve Yegge's."
               :type git  
               :url "git://github.com/mooz/js2-mode.git"  
               :load "js2-mode.el"  
               :compile ("js2-mode.el")  
               :features js2-mode)
        
        (:name kill-ring-search
               :description "Search the kill ring in the minibuffer."
               :type elpa
               :post-init (lambda()
                            (global-set-key "\M-\C-y" 'kill-ring-search)))
        
        (:name multi-term
               :description "Manage multiple terminal buffers. Plays nicely with gnuscreen."
               :type git
               :url "https://github.com/emacsmirror/multi-term"
               :load "multi-term.el"
               :post-init (lambda()
                            (autoload 'multi-term-next "multi-term" nil t)
                            (setq multi-term-program "/bin/bash")
                            (setq explicit-bash-args (quote ("--noediting" "-i" "-l")))
                            (setq exec-path
                                  (quote ("/usr/bin"
                                          "/bin"
                                          "/usr/sbin"
                                          "/sbin"
                                          "/usr/local/bin"
                                          "/usr/X11/bin"
                                          "/opt/local/bin"
                                          "/usr/local/git/bin"
                                          "/Applications/Emacs.app/Contents/MacOS/bin"
                                          "/Users/jstautz/bin")))
                            ;; for autopair
                            (add-hook 'term-mode-hook
                                      #'(lambda () (setq autopair-dont-activate t)))
                            (global-set-key (kbd "C-c t") 'multi-term-next)
                            (global-set-key (kbd "C-c T") 'multi-term)
                            (setq term-bind-key-alist
                                  (quote (("C-c C-c" . term-interrupt-subjob)
                                          ("C-p" . previous-line)
                                          ("C-n" . next-line)
                                          ("C-s" . isearch-forward)
                                          ("C-r" . isearch-backward)
                                          ("C-m" . term-send-raw)
                                          ("M-f" . term-send-forward-word)
                                          ("M-b" . term-send-backward-word)
                                          ("M-o" . term-send-backspace)
                                          ("M-p" . term-send-up)
                                          ("M-n" . term-send-down)
                                          ("M-M" . term-send-forward-kill-word)
                                          ("M-N" . term-send-backward-kill-word-old)
                                          ("M-r" . term-send-reverse-search-history)
                                          ("M-," . term-send-input)
                                          ("M-." . comint-dynamic-complete)
                                          ("M-" . term-send-backward-kill-word)
                                          ("M-d" . term-send-forward-kill-word-partial))))))
        
        (:name psvn
               :description "svn interface for Emacs"
               :type git
               :url "git://github.com/emacsmirror/psvn.git"
               :load "psvn.el"
               :compile ("psvn.el")
               :features psvn
               :post-init (lambda()
                            (setq vc-path (quote ("/sw/bin")))))

        (:name remember
               :description "A mode for quickly jotting down things to remember"
               :type git
               :url "http://repo.or.cz/r/remember-el.git"
               :compile ("remember.el")
               :features remember)
        
        (:name restclient
               :description "Manually explore and test HTTP REST webservices"
               :type git
               :url "git://github.com/pashky/restclient.el.git"
               :load ("json-reformat.el" "restclient.el")
               :compile ("json-reformat.el" "restclient.el")
               :features restclient)
               
        (:name smex
               :description "M-x interface with Ido-style fuzzy matching."
               :type git
               :url "http://github.com/nonsequitur/smex.git"
               :features smex
               :post-init (lambda ()
                            (smex-initialize)
                            (global-set-key (kbd "M-x") 'smex)
                            (global-set-key (kbd "M-X") 'smex-major-mode-commands)
                            (global-set-key "\C-x\C-m" 'smex)
                            (global-set-key "\C-c\C-m" 'smex)
                            (add-hook 'org-mode-hook
                                                (lambda()
                                                  (define-key org-mode-map
                                                    (kbd "\C-c\C-m") 'smex)))
                            (add-hook 'org-mode-hook
                                      (lambda()
                                        (define-key org-mode-map
                                          (kbd "\C-x\C-m") 'smex)))
                            ;; This is your old M-x.
                            (global-set-key
                             (kbd "C-c C-c M-x") 'execute-extended-command)))
        (:name yas
               :description "Yet Another Snippet"
               :type git
               :url "git://github.com/capitaomorte/yasnippet.git"
               :features "yasnippet"
               :post-init (lambda()
                            (yas/initialize)
                            (yas/load-directory (concat dotemacs-dir "/snippets/"))
                            ;; Yasnippet's tab-completion should override org-mode's
                            (add-hook 'org-mode-hook 
                                      #'(lambda () 
                                          (local-set-key [tab] 'yas/expand)))
                            ;; Also override markdown-mode's tab
                            (add-hook 'markdown-mode-hook 
                                      #'(lambda () 
                                          (local-set-key [tab] 'yas/expand))))
               :submodule nil)
        ))


;; list all packages you want installed  
(setq my-el-get-packages  
      (append  
       '(
         ace-jump-mode
         android
         android-mode
         browse-kill-ring
         css-mode
         edb
         elip
         ido
         js2-mode-mooz
         kill-ring-search
         markdown-mode
         multi-term
         nxhtml
         org-mode
         psvn
         remember
         restclient
         smex
         yas
        )  
       (mapcar 'el-get-source-name el-get-sources)))
(el-get 'sync my-el-get-packages)

;; TODO look at https://github.com/technomancy/ido-ubiquitous/blob/master/ido-ubiquitous.el
;;   if you can use regular smex


(setq tramp-default-method "ssh")


;;-----------------------------------------------------------------------------
;; Org-mode -- This right here is the reason to use emacs
;;             (forgive the gnarly messy setup below -- lots of custom configs)
;;-----------------------------------------------------------------------------

(setq org-directory (concat home-dir "org/"))
(setq writing-directory (concat home-dir "Documents/Writing/"))

;; Standard org-mode setup
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; highlight current line when in agenda
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;; My agenda setup (I like managing it here rather than thru custom)
(setq org-agenda-files (quote ("~/org/flagged.org"
                               "~/org/inbox.txt"
                               "~/org/projects.org")))

;; Search on other files, too
(setq org-agenda-text-search-extra-files '("~/org/goals.org"
                                           "~/org/someday_maybe.org"
                                           "~/org/notes/tech_log.txt"
                                           "~/org/notes/reference.org"))

(setq org-stuck-projects (quote ("+LEVEL=2-REFILE-UNFILED-HABITS/-DONE"
  ("TODO" "NEXT" "STARTED") ("NOTES") "")))

(setq org-agenda-custom-commands
           '(
             (" " "Agenda overview"
              ((agenda"")
			   (tags "REFILE" 
					 ((org-agenda-overriding-header "Tasks to Refile")))
			   (org-agenda-list-stuck-projects) 
			   (tags-todo "-REFILE+Focus=\"\""
						  ((org-agenda-overriding-header "Tasks to Estimate")
						   (org-agenda-skip-function 'bh/skip-projects)
						   (org-tags-match-list-sublevels t)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   (tags-todo "-REFILE/!NEXT|STARTED"
			    		  ((org-agenda-overriding-header "Next Tasks")
			    		   (org-agenda-skip-function 'bh/skip-projects)
			    		   (org-agenda-todo-ignore-scheduled 'future)
			    		   (org-tags-match-list-sublevels t)
			    		   (org-agenda-sorting-strategy
			    			'(todo-state-down effort-up category-keep))))
			   (tags-todo "@work-REFILE"
						  ((org-agenda-overriding-header "Work Tasks")
						   (org-tags-match-list-sublevels t)
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   (tags-todo "@home-REFILE"
						  ((org-agenda-overriding-header "Home Tasks")
						   (org-tags-match-list-sublevels t)
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   (tags-todo "@desk-REFILE"
						  ((org-agenda-overriding-header "Writing/Focused Tasks")
						   (org-tags-match-list-sublevels t)
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   (tags-todo "@cpu-REFILE"
						  ((org-agenda-overriding-header "Computer Tasks")
						   (org-tags-match-list-sublevels t)
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   (tags-todo "@errands-REFILE"
						  ((org-agenda-overriding-header "Errands")
						   (org-tags-match-list-sublevels t)
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   (tags-todo "@transit-REFILE"
						  ((org-agenda-overriding-header "Transit Tasks")
						   (org-tags-match-list-sublevels t)
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   ))
			 ("c" "@cpu + calendar"
              ((agenda "")
			   (tags-todo "@cpu-REFILE" 
						  ((org-agenda-overriding-header "All Computer Tasks")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
               ))
			 ("d" "@desk"
              ((tags-todo "@desk-REFILE" 
						  ((org-agenda-overriding-header "All Desk/Writing Tasks")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
              ))
             ("h" "@home + calendar"
              ((agenda "")
			   (tags-todo "@home-REFILE" 
						  ((org-agenda-overriding-header "All Home Tasks")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   ))
             ("r" "@errands" tags-todo "@errands")
             ("z" "@transit" tags-todo "@transit")
             ("w" "@work + agenda"
              ((agenda "")
			   (tags-todo "@work-REFILE" 
						  ((org-agenda-overriding-header "All Work Tasks")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))

			   ))
			 ))

;; A couple helper functions for org agendas from Bernt Hansen
;; http://doc.norang.ca/org-mode.html

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (let ((has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t)))
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (member (org-get-todo-state) org-todo-keywords-1)
          (setq has-subtask t))))
    (and is-a-task has-subtask)))

(defun bh/skip-projects ()
  "Skip trees that are projects"
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (cond
     ((bh/is-project-p)
      next-headline)
     (t
      nil))))

;; Resume org clocks if I restart emacs w/ running clock
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; Change task to STARTED when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-started)

(defun bh/clock-in-to-started (kw)
  "Switch task from TODO or NEXT to STARTED when clocking in.
Skips capture tasks."
  (if (and (member (org-get-todo-state) (list "TODO" "NEXT"))
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      "STARTED"))


;; using remember with org-mode (deprecated, but I like it more than org-capture)
(org-remember-insinuate)
(setq org-default-notes-file (concat org-directory "inbox.txt"))
(define-key global-map "\C-cr" 'org-remember)

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; remember templates for org-remember:
;;   t = new TODO item
;;   j = new journal entry (dated today)
;;   i = new inbox item
;;   q = only used via QuickSilver -- new inbox item
     (setq org-remember-templates
      '(("Todo" ?t "*** TODO %^{Action to be taken} %^G\n    %?\n    Added: %U" (concat org-directory "projects.org") "Unfiled")
        ("Journal" ?j "* %U %^{Title} %^g\n\n%?\n" (concat writing-directory "01-composting/journal.txt") top)
        ("Inbox" ?i "* %U %^{Title}\n  %i%?\n   %a\n\n" (concat org-directory "inbox.txt") top)
        ("oldQS remember" ?f "* %U %?\n%i\n%a\n\n" (concat org-directory "inbox.txt") top)
        ("QS remember" ?q "* %i\n\n  Added: %U" (concat org-directory "inbox.txt") top)
        ("AppleScript remember" ?y "* %:shortdesc\n  %:initial\n   Source: %u, %c\n\n  %?" (concat org-directory "inbox.txt"))
        ("AppleScript note" ?z "* %?\n\n  Date: %u\n" (concat org-directory "inbox.org"))))

(require 'org-annotation-quicksilver)

;; org-mac-protocol is replacing oaq (eh, haven't got this to work yet)
;;(require 'org-mac-protocol)

;; trying org-capture instead of remember -- haven't really got too solidly into this yet.
(setq org-capture-templates
	  '(("t" "Todo" entry
		 (file+headline (concat org-directory "projects.org") "Unfiled")
		 "*** TODO %^{Action to be taken} %^G\n    %?\n    Added: %U" :prepend t)
		("j" "Journal" entry
		 (file (concat writing-directory "01-composting/journal.txt"))
		 "* %U %^{Title} %^g\n\n%?\n" :prepend t)
		("i" "Inbox" entry
		 (file (concat org-directory "inbox.txt"))
		 "* %U %^{Title}\n  %i%?\n   %a\n\n" :prepend t)
		("q" "QS remember" entry
		 (file (concat org-directory "inbox.txt"))
		 "* %U %?\n%i\n%a\n\n" :prepend t)
		("y" "AppleScript remember" entry
		 (file+headline
		  (concat org-directory "inbox.txt")
		  "")
		 "* %:shortdesc\n  %:initial\n   Source: %u, %c\n\n  %?" :prepend t)
		("z" "AppleScript note" entry
		 (file+headline
		  (concat org-directory "inbox.txt")
		  "")
		 "* %?\n\n  Date: %u\n" :prepend t)
		("q" "QS Inbox" entry
		 (file+headline
		  (concat org-directory "inbox.txt")
		  "Inbox")
		 "\n* QS %U%?\n%i\n%a" :prepend t)))

;; remind me of any appointments
(appt-activate 1) 


;; org-mobile settings
(setq org-mobile-files '("~/org/projects.org""~/org/inbox.txt" "~/org/notes/book_list.org" "~/org/goals.org" "~/org/someday_maybe.org"))
(setq org-mobile-inbox-for-pull (concat org-directory "flagged.org"))
(setq org-mobile-directory (concat home-dir "Dropbox/MobileOrg"))
(setq org-mobile-use-encryption t)
;; I don't care about possible leakage in autosave files
(setq org-crypt-disable-auto-save nil)

(defun jcs:org-mobile-decrypt-creds ()
  (interactive)
  (require 'secrets))

(add-hook 'org-mobile-pre-push-hook 'jcs:org-mobile-decrypt-creds)
(add-hook 'org-mobile-pre-pull-hook 'jcs:org-mobile-decrypt-creds)


;; org keybindings
(global-set-key (kbd "<f5>") 'org-narrow-to-subtree)
(global-set-key (kbd "<M-f5>") 'jcs:org-todo-tree)
(global-set-key (kbd "<S-f5>") 'jcs:widen)
(global-set-key (kbd "<f6>") 'org-clock-goto)

(defun jcs:org-todo-tree ()
  (interactive)
  (org-narrow-to-subtree)
  (org-show-todo-tree nil))

(defun jcs:widen ()
  (interactive)
  (widen)
  (org-reveal)
  (org-remove-occur-highlights))


;; Trying some things for more focused time tracking using org-mode timer
(add-to-list 'org-modules 'org-timer)
(setq org-timer-default-timer 25)

;; Um. Yeah. Not sure why, but yeah.
(org-reload)

;;-----------------------------------------------------------------------------
;; Diary and appt settings (integrates with org-mode)
;;-----------------------------------------------------------------------------
(setq diary-file (concat org-directory "calendar.diary"))
(add-hook 'diary-display-hook 'fancy-diary-display)
(setq list-diary-entries-hook
      '(include-other-diary-files sort-diary-entries))
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)

(require 'appt)
(setq org-agenda-include-diary t)
(setq appt-time-msg-list nil)
(org-agenda-to-appt)

;; Re-load agenda dates/items into appt whenever I load agenda view
(defadvice  org-agenda-redo (after org-agenda-redo-add-appts)
  "Pressing `r' on the agenda will also add appointments."
  (progn 
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt)))

(ad-activate 'org-agenda-redo)

;; Reset the appointments every day at one minute after midnight
(run-at-time "24:01" 86400 'org-agenda-redo)

;; Set Agenda view to show Habits again each day at 4am
(run-at-time "04:00" 86400 '(lambda () (setq org-habit-show-habits t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom functions, aliases
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;-----------------------------------------------------------------------------
;; wdired toggle -- for rewriting filenames from dired mode
;;-----------------------------------------------------------------------------
(eval-after-load 'dired
  '(define-key dired-mode-map "r"
     'wdired-change-to-wdired-mode))


;;-----------------------------------------------------------------------------
;; Set up Emacs->Growl notifications
;;-----------------------------------------------------------------------------
(defvar growl-program "/usr/local/bin/growlnotify")

(defun growl (title message &optional id)
  (if (eq id nil)
      (start-process "growl" " growl"
                     growl-program title "-w" "-a" "iCal.app")
    (start-process "growl" " growl"
                   growl-program title "-w" "-a" "iCal.app" "-d" id))
  (process-send-string " growl" message)
  (process-send-string " growl" "\n")
  (process-send-eof " growl"))


;;-----------------------------------------------------------------------------
;; Send Appt reminders to Growl
;;-----------------------------------------------------------------------------
(progn
  (appt-activate 1)
  (setq appt-display-format 'window)
  (setq appt-disp-window-function (function my-appt-disp-window))
  (defun my-appt-disp-window (min-to-app new-time msg)
    (growl "Reminder" (format "%s" msg))))


;;-----------------------------------------------------------------------------
;; jcs:getcals -- Sync Google Calendars to emacs diary
;;-----------------------------------------------------------------------------
(require 'icalendar)

(defun getcal (url)
  "Download ics file and add to diary"
  (let ((tmpfile (url-file-local-copy url)))
    (icalendar-import-file tmpfile "~/org/calendar.diary" t)
    (kill-buffer (car (last (split-string tmpfile "/"))))
    )
  )
(setq google-calendars '(
                         "http://www.google.com/calendar/ical/jeff.stautz%40gmail.com/private-022fa6b52c7455e86ea246b7585220df/basic.ics"
                         "http://www.google.com/calendar/ical/g7afrnp17fakgmhp5qfuq9l2e0%40group.calendar.google.com/private-7765541dea7c847c9d8d3916df7e6373/basic.ics"
                         "http://www.google.com/calendar/ical/88cucc6oldmgbnfc5nc7np67so%40group.calendar.google.com/private-6559c41ff64779ce15b2b10aa6038fd3/basic.ics"
                         "http://www.google.com/calendar/ical/j7scthcsk7r0dfo9qgr07335lo%40group.calendar.google.com/private-4943b58ad16ca4126d1ba2e0200a01de/basic.ics"
                         "https://www.google.com/calendar/ical/jeff.stautz%40hootsuite.com/private-d8a2ab2011000563e2acb53e06764b3a/basic.ics"
                         ))
(defun jcs:getcals ()
  (interactive)
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
	  (setq filepath "/Users/jstautz/org/projects.org")
	  (setq filename (file-name-nondirectory filepath))
	  (setq mybuffer (find-file filepath))
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
;; Remove empty clock drawers on logout
;;-----------------------------------------------------------------------------
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "CLOCK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


;; i-search with initial contents.
;; original source: http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Post-flight
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns-toggle-fullscreen)
(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                             (- (+ hi lo) (+ (first *emacs-load-start*) (second
                             *emacs-load-start*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom-set-variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-M-RET-may-split-line t)
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3)))
 '(org-agenda-dim-blocked-tasks t)
 '(org-agenda-include-diary t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-sorting-strategy (quote ((agenda time-up priority-down) (todo priority-down) (tags priority-down))))
 '(org-agenda-span (quote week))
 '(org-agenda-start-with-clockreport-mode nil)
 '(org-agenda-start-with-follow-mode nil)
 '(org-agenda-tags-column -120)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-scheduled (quote future))
 '(org-agenda-todo-list-sublevels t)
 '(org-archive-location (concat org-directory "archives.org::"))
 '(org-archive-mark-done nil)
 '(org-attach-auto-tag nil)
 '(org-babel-no-eval-on-ctrl-c-ctrl-c t)
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item))))
 '(org-clock-history-length 5)
 '(org-clock-idle-time 5)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state (quote bh/clock-in-to-started))
 '(org-clock-into-drawer t)
 '(org-clock-modeline-total (quote current))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-persist t)
 '(org-clock-sound nil)
 '(org-combined-agenda-icalendar-file (concat org-directory "org.ics"))
 '(org-completion-use-ido t)
 '(org-confirm-shell-link-function (quote y-or-n-p))
 '(org-default-priority 69)
 '(org-enforce-todo-checkbox-dependencies nil)
 '(org-enforce-todo-dependencies nil)
 '(org-export-with-TeX-macros nil)
 '(org-fast-tag-selection-single-key t)
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . default) ("\\.celtx\\'" . system) ("\\.doc\\'" . system) ("\\.xls\\'" . system) ("\\.fdr\\'" . system) ("\\.dvi\\'" . system))))
 '(org-fontify-done-headline t)
 '(org-global-properties (quote (("Effort_ALL" . "0:05 0:10 0:15 0:25 1:00 2:00 4:00 8:00") ("Focus_ALL" . "High Medium Low"))))
 '(org-habit-show-habits-only-for-today nil)
 '(org-hide-leading-stars t)
 '(org-icalendar-combined-name "Org")
 '(org-icalendar-include-todo t)
 '(org-icalendar-store-UID t)
 '(org-id-include-domain nil)
 '(org-id-method (quote uuidgen))
 '(org-level-color-stars-only t)
 '(org-log-done (quote time))
 '(org-log-into-drawer "LOGBOOK")
 '(org-log-redeadline (quote note))
 '(org-log-reschedule (quote note))
 '(org-lowest-priority 69)
 '(org-modules (quote (org-bbdb org-bibtex org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-habit)))
 '(org-odd-levels-only t)
 '(org-priority-start-cycle-with-default t)
 '(org-refile-targets (quote ((("~/org/projects.org" "~/org/someday_maybe.org") :maxlevel . 2))))
 '(org-return-follows-link t)
 '(org-reverse-note-order (quote (("inbox" . t))))
 '(org-show-following-heading (quote ((default))))
 '(org-show-hierarchy-above (quote ((default . t) (tags-tree))))
 '(org-show-notification-handler (quote (lambda (notification) (growl "org-mode notification" notification))))
 '(org-show-siblings (quote ((default) (isearch t))))
 '(org-special-ctrl-a/e t)
 '(org-table-export-default-format "orgtbl-to-csv")
 '(org-tags-column 80)
 '(org-use-speed-commands t))

