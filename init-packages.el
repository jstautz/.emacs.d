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
(add-to-list 'load-path (concat dotemacs-dir "el-get/el-get"))

;; el-get, install thyself
(unless (require 'el-get nil t)
 (url-retrieve
  "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
  (lambda (s)
    (let ((el-get-master-branch) (el-get-install-skip-emacswiki-recipes))
      (goto-char (point-max))
      (eval-print-last-sexp)))))

(setq el-get-dir (concat dotemacs-dir "el-get")
      el-get-user-package-directory (concat dotemacs-dir "config"))


;; el-get Recipes 
(setq el-get-sources
      '(

        (:name android
               :description "Included with Android SDK - connect Emacs to emulator"
               :type http
               :url "https://raw.github.com/evnm/emacs/master/vendor/android.el"
               :compile ("android.el")
               :features android)
        
;; TODO -- fix this build script so it performs a sed replace of ECUKES_EL and ECUKES_EMACS vars on install.
        (:name ecukes
               :description "Cucumber integration testing framework for Emacs"
               :type git
               :url "https://github.com/rejeep/ecukes"
               :build ("ln -s ~/.emacs.d/el-get/ecukes/ecukes ~/Dropbox/bin/ecukes"))

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

        (:name espuds
               :description "Ecukes step definitions"
               :type git
               :url "https://github.com/rejeep/espuds")

        (:name htmlize
               :description "Convert buffer text and decorations to HTML."
               :type git
               :url "http://fly.srk.fer.hr/~hniksic/emacs/htmlize.git")
        
        (:name ido
               :description "Interactively Do Things. Fuzzy match buffers/files/etc."
               :type http
               :url "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/ido.el")

        (:name impatient-mode
               :description "See your html rendered in browser as yout type"
               :type git
               :url "https://github.com/netguy204/imp.el.git")
        
        (:name kanban
               :description "Parse org-todo headlines to use org-tables as Kanban tables"
               :type http
               :url "https://bitbucket.org/ArneBab/kanban.el/raw/ce256090a5a4ef6db00a8ba78d9bd5838571d4ea/kanban.el"
               :depends org-mode
               :features kanban)
        
        (:name kill-ring-search
               :description "Search the kill ring in the minibuffer."
               :type elpa)

;; TODO troubleshoot this a bit
        (:name org-annotation-quicksilver
               :description "Allows creation of org notes from other applications via Quicksilver and remember-mode."
               :type git
               :url "https://github.com/jstautz/org-mac-protocol.git"
               :depends org-mode)

        (:name restclient
               :description "Manually explore and test HTTP REST webservices"
               :type git
               :url "git://github.com/pashky/restclient.el.git"
               :load ("json-reformat.el" "restclient.el")
               :compile ("json-reformat.el" "restclient.el")
               :features restclient)

        (:name remember
               :description "A mode for quickly jotting down things to remember"
               :type git
               :url "http://repo.or.cz/r/remember-el.git"
               :compile ("remember.el")
               :features remember)

        (:name simple-httpd
               :description "A simple Emacs webserver"
               :type git
               :url "https://github.com/skeeto/emacs-http-server.git")

        (:name unbound
               :description "Find convenient unbound keybindings."
               :type http
               :url "http://www.emacswiki.org/emacs/download/unbound.el"
               :features unbound)

        (:name wc-mode
               :description "Running word count with goals (minor mode)"
               :type git
               :url "https://github.com/bnbeckwith/wc-mode.git"
               :features wc-mode)
        
        (:name wgrep
               :description "Writable grep buffer, apply the changes to files."
               :type git
               :url "https://github.com/magnars/Emacs-wgrep.git"
               :features wgrep)

        (:name writer-names
               :description "Generate random names (for fiction writers)"
               :type git
               :url "https://github.com/jstautz/writer-names.git"
               :before (progn
                         (defvar writer-male-names
                           (concat el-get-dir "/writer-names/census_data/dist.male.first"))
                         (defvar writer-female-names
                           (concat el-get-dir "/writer-names/census_data/dist.female.first"))
                         (defvar writer-last-names
                           (concat el-get-dir "/writer-names/census_data/dist.all.last")))
               :load "writer-names.el")
               
              ))

;; All packages to install
;; TODO -- troubleshoot/test multi-term (yanking seemed weird) -- maybe problem with kill?
(unwind-protect
    (let (retval)
      (condition-case ex
          (setq retval (setq my-packages
			     (append
			      '(ace-jump-mode
                    android-mode
                    applescript-mode
                    auto-complete
                    auto-complete-css
                    auto-complete-emacs-lisp
                    auto-complete-yasnippet
                    browse-kill-ring
                    css-mode
                    diminish
                    edb
                    elisp-slime-nav
                    ;;ensime
                    expand-region
                    feature-mode
                    git-modes
                    ghc-mod
                    haskell-mode
                    ido-ubiquitous
                    js2-mode
                    magit
                    markdown-mode
                    multi-term
                    multiple-cursors
                    nxhtml
                    org-mode
                    psvn
                    rainbow-mode
                    scala-mode2
                    smex
                    smooth-scrolling
                    undo-tree
                    yaml-mode
                    yasnippet)
			      (mapcar 'el-get-source-name el-get-sources))))
        ('error (message (format "Caught exception: [%s]" ex))))
        retval)
  (message "Attempting to build el-get package list..."))



;; Sync packages. Wrapped in unwind-protect block in case el-get not yet installed
(unwind-protect
    (let (retval)
      (condition-case ex
          (setq retval (el-get 'sync my-packages))
        ('error (message (format "Caught exception: [%s]" ex))))
        retval)
  (message "Attempting el-get sync..."))
 
(provide 'init-packages)
