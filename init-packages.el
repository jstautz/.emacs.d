;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up and install external packages using el-get and package.el sources
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Add a bunch of other repos to my sources list
(require 'package)
(dolist (source '( ("gnu" . "http://elpa.gnu.org/packages/")
                   ("elpa" . "http://tromey.com/elpa/")
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


;; My custom el-get recipes 
(setq el-get-sources
      '(
        (:name ace-jump-mode
               :website "https://github.com/winterTTr/ace-jump-mode/wiki"
               :description "A quick cursor location minor mode for emacs"
               :type github
               :pkgname "winterTTr/ace-jump-mode"
               :features ace-jump-mode ace-jump-mode-pop-mark)

        (:name commander
               :type github
               :description "Emacs command line parser"
               :pkgname "rejeep/commander.el"
               :depends (f s dash ansi)
               :features commander)
        
        (:name ido
               :description "Interactively Do Things. Fuzzy match buffers/files/etc."
               :type http
               :url "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/ido.el")

        (:name kill-ring-search
               :description "Search the kill ring in the minibuffer."
               :type elpa)

        (:name gmail-message-mode
               :description "Edit Gmail messages with markdown syntax. For use with Chrome 'Edit with Emacs' extension."
               :type github
               :pkgname "Bruce-Connor/gmail-mode"
               :depends (ham-mode)
               :features gmail-message-mode)

        (:name ham-mode
               :description "Seamlessly edit an html file using markdown."
               :type github
               :pkgname "Bruce-Connor/ham-mode"
               :depends (html-to-markdown markdown-mode)
               :features ham-mode)

        (:name html-to-markdown
               :description ""
               :type github
               :pkgname "Bruce-Connor/html-to-markdown"
               :features html-to-markdown)
        
        (:name restclient
               :description "Manually explore and test HTTP REST webservices"
               :type git
               :url "git://github.com/pashky/restclient.el.git"
               :load ("json-reformat.el" "restclient.el")
               :compile ("json-reformat.el" "restclient.el")
               :features restclient)

        (:name smart-mode-line
               :description "Sexy mode-line for Emacs"
               :type github
               :pkgname "Bruce-Connor/smart-mode-line"
               :depends (dash)
               :features smart-mode-line)
        
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

;; Packages to install from base el-get recipes
(setq my-packages
      (append
       '(
         ansi
         browse-kill-ring
         dash
         diminish
         edit-server
         expand-region
         f
         ido-ubiquitous
         js2-mode
         js2-refactor
         magit
         markdown-mode
         multiple-cursors
         org-mode
         psvn
         s
         smex
         yasnippet
         )
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

(provide 'init-packages)

