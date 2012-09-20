;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up and install external packages using el-get and package.el sources
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Add a bunch of other repos to my sources list
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


;; Recipes for el-get packages
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

        (:name applescript-mode
               :description "Major mode for editing AppleScript source"
               :type git
               :url "https://github.com/kalifg/applescript-mode"
               :features applescript-mode)
                
        (:name browse-kill-ring
               :description "View kill-ring with M-y"
               :type elpa
               :post-init (lambda()
                            (browse-kill-ring-default-keybindings)))
        
        (:name css-mode :type elpa)

        (:name cucumber
               :description "Emacs mode for editing plain text user stories"
               :type git
               :url "https://github.com/michaelklishin/cucumber.el"
               :features feature-mode
               :post-init (lambda()
                            (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))))

        (:name ecukes
               :description "Cucumber integration testing framework for Emacs"
               :type git
               :url "https://github.com/rejeep/ecukes"
               :build `(,"ln -s ~/.emacs.d/el-get/ecukes/ecukes ~/Dropbox/bin/ecukes"))

        (:name espuds
               :description "Ecukes step definitions"
               :type git
               :url "https://github.com/rejeep/espuds")

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

        (:name expand-region
               :description "Increase selected region by semantic units."
               :type git
               :url "https://github.com/magnars/expand-region.el"
               :features expand-region
               :post-init (lambda()
                            (global-set-key (kbd "C-=") 'er/expand-region)))
        
        (:name ido
               :description "Interactively Do Things. Fuzzy match buffers/files/etc."
               :type http
               :url "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/ido.el"
               :compile ("ido.el")
               :post-init (lambda()
                            (ido-mode t)
                            (setq ido-enable-flex-matching t
                                  ido-enable-tramp-completion nil
                                  ido-everywhere t
                                  ido-is-tramp-root nil
                                  ido-max-prospects 10
                                  ido-record-ftp-work-directories nil
                                  ido-show-dot-for-dired t
                                  ido-use-virtual-buffers t)
                            (recentf-mode t)
                            (put 'ido-exit-minibuffer 'disabled nil)))

        (:name ido-ubiquitous
               :description "Use ido (nearly) everywhere."
               :type elpa
               :depends ido)
        
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

        (:name mark-multiple
               :description "An emacs extension that sorta lets you mark several regions at once."
               :type git
               :url "https://github.com/magnars/mark-multiple.el.git"
               :features ("inline-string-rectangle" "mark-more-like-this" "rename-sgml-tag")
               :post-init (lambda()
                           (global-set-key (kbd "C-x r t") 'inline-string-rectangle)
                           (global-set-key (kbd "C-<") 'mark-previous-like-this)
                           (global-set-key (kbd "C->") 'mark-next-like-this)
                           (global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
                           (global-set-key (kbd "C-*") 'mark-all-like-this)
                          ;; (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)
))

        (:name multi-term
               :description "Manage multiple terminal buffers. Plays nicely with gnuscreen."
               :type git
               :url "https://github.com/emacsmirror/multi-term"
               :load "multi-term.el"
               :post-init (lambda()
                            (autoload 'multi-term-next "multi-term" nil t)
                            (setq multi-term-program "/bin/bash")
                            (setq explicit-bash-args (quote ("--noediting" "-i" "-l")))
                            ;; for autopair
                            (add-hook 'term-mode-hook
                                      #'(lambda () (setq autopair-dont-activate t)))
                            (global-set-key (kbd "C-c t") 'multi-term-next)
                            (global-set-key (kbd "C-c T") 'multi-term)
                            (setq term-bind-key-alist
                                  (quote (("C-y" . my-term-paste)
                                          ("C-c C-c" . term-interrupt-subjob)
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

        (:name multiple-cursors
               :description "An experiment in multiple cursors for emacs. Still very much in beta."
               :type git
               :url "https://github.com/magnars/multiple-cursors.el.git"
               :features ("multiple-cursors" "multiple-cursors-core"
                          "mc-mark-multiple-integration"
                          "mc-edit-lines")
               :post-init (lambda()
                            ;; Experimental multiple-cursors
                            (global-set-key (kbd "C-x a e") 'mc/edit-ends-of-lines)
                            (global-set-key (kbd "C-x a a") 'mc/edit-beginnings-of-lines)))
        
        (:name php+-mode
               :description "A better PHP mode with Zend Framework support"
               :type elpa
               :features php+-mode
               :post-init (lambda()
                            (require 'php+-mode)
                            (php+-mode-setup)))
        
        (:name psvn
               :description "svn interface for Emacs"
               :type git
               :url "git://github.com/emacsmirror/psvn.git"
               :load "psvn.el"
               :compile ("psvn.el")
               :features psvn
               :post-init (lambda()
                            (setq vc-path (quote ("/sw/bin")))))

        (:name rainbow-mode
               :description "Colorize color names in buffers"
               :type git
               :url "https://github.com/emacsmirror/rainbow-mode"
               :features rainbow-mode)
        
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
               :features smex)

        (:name unbound
               :description "Find convenient unbound keybindings."
               :type http
               :url "http://www.emacswiki.org/emacs/download/unbound.el"
               :features unbound)

        (:name wgrep
               :description "Writable grep buffer, apply the changes to files."
               :type git
               :url "https://github.com/magnars/Emacs-wgrep.git"
               :features wgrep
               :post-init (lambda()
                            (setq wgrep-enable-key "r")))

        ;; TODO -- pulling in snips from yas git, plus cucumber git, plus my own.
        ;;  need to manage/consolidate these better.
        
        (:name yas
               :description "Yet Another Snippet"
               :type git
               :url "git://github.com/capitaomorte/yasnippet.git"
               :features "yasnippet"
               :post-init (lambda()
                            (yas/initialize)
                            (yas/load-directory (concat dotemacs-dir "/snippets/"))
                            (yas/load-directory (concat dotemacs-dir "/el-get/cucumber/snippets"))
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

;; All external packages to install & sync with el-get
(setq my-el-get-packages  
      (append  
       '(
         ace-jump-mode
         android
         android-mode
         applescript-mode
         browse-kill-ring
         css-mode
         cucumber
         ecukes
         espuds
         edb
         elip
         expand-region
         ido
         ido-ubiquitous
         js2-mode-mooz
         kill-ring-search
         magit
         magithub
         mark-multiple
         markdown-mode
         multi-term
         multiple-cursors
         nxhtml
         org-mode
         php+-mode
         psvn
         remember
         restclient
         smex
         smooth-scrolling
         unbound
         undo-tree
         wgrep
         yas
         yaml-mode
        )  
       (mapcar 'el-get-source-name el-get-sources)))
(el-get 'sync my-el-get-packages)

;;-----------------------------------------------------------------------------
;; Additional package setup
;;-----------------------------------------------------------------------------

;; Don't load smex until I try to use it
(defun jcs:smex-init ()
  (interactive)
  (condition-case description
      (progn
        (smex-initialize)
        (global-set-key (kbd "M-x") 'smex)
        (global-set-key (kbd "M-X") 'smex-major-mode-commands)
        (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
        (add-hook 'org-mode-hook
                  (lambda()
                    (define-key org-mode-map
                      (kbd "\C-c\C-m") 'smex)))
        (add-hook 'org-mode-hook
                  (lambda()
                    (define-key org-mode-map
                      (kbd "\C-x\C-m") 'smex)))
        (smex))
    (error (execute-extended-command))))
(global-set-key (kbd "M-x") 'jcs:smex-init)


(provide 'init-packages)
