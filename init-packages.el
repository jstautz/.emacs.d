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

        (:name puzzlemacs
               :description "Emacs editing challenges"
               :type svn
               :url "http://lsvn.lysator.liu.se/svnroot/puzzlemacs"
               :load "game.el"
               :compile ("game.el"))
               
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

;; All external packages to install & sync with el-get
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
         magit
         magithub
         markdown-mode
         multi-term
         nxhtml
         org-mode
         psvn
         puzzlemacs
         remember
         restclient
         smex
         yas
        )  
       (mapcar 'el-get-source-name el-get-sources)))
(el-get 'sync my-el-get-packages)

(provide 'init-packages)
