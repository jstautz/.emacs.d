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
;;         will also need to troubleshoot setup again.
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
        
        (:name ido
               :description "Interactively Do Things. Fuzzy match buffers/files/etc."
               :type http
               :url "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/ido.el")

        (:name kill-ring-search
               :description "Search the kill ring in the minibuffer."
               :type elpa)

        (:name multiple-cursors
               :description "An experiment in multiple cursors for emacs. Still very much in beta."
               :type git
               :url "https://github.com/magnars/multiple-cursors.el.git"
               :features ("multiple-cursors" "multiple-cursors-core"
                          "mc-mark-multiple-integration"
                          "mc-edit-lines"))
        
;; TODO -- add init-powerline.el config file that changes some faces
        (:name powerline
               :description "Emacs version of the Vim powerline."
               :type git
               :url "git://github.com/milkypostman/powerline.git"
               :load "powerline.el"
               :compile ("powerline.el"))

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

        (:name unbound
               :description "Find convenient unbound keybindings."
               :type http
               :url "http://www.emacswiki.org/emacs/download/unbound.el"
               :features unbound)

        (:name wgrep
               :description "Writable grep buffer, apply the changes to files."
               :type git
               :url "https://github.com/magnars/Emacs-wgrep.git"
               :features wgrep)
        
              ))

;; All packages to install

;; TODO -- troubleshoot/test multi-term (yanking seemed weird)
;; TODO -- fix nxhtml
;; TODO -- install and fix yas

(unwind-protect
    (let (retval)
      (condition-case ex
          (setq retval (setq my-packages
			     (append
			      '(ace-jump-mode
                    android-mode
                    applescript-mode
                    browse-kill-ring
                    css-mode
                    edb
                    expand-region
                    feature-mode
                    ido-ubiquitous
                    js2-mode
                    magit
                    mark-multiple
                    markdown-mode
                    multi-term
;;                    nxhtml
                    org-mode
                    psvn
                    rainbow-mode
                    smex
                    smooth-scrolling
                    undo-tree
                    yaml-mode)
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
