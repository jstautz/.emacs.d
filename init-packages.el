;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up and install external packages using el-get and package.el sources
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; My custom el-get recipes 
(setq el-get-sources
      '(
        (:name ace-jump-mode
               :website "https://github.com/winterTTr/ace-jump-mode/wiki"
               :description "A quick cursor location minor mode for emacs"
               :type github
               :pkgname "winterTTr/ace-jump-mode"
               :features ace-jump-mode ace-jump-mode-pop-mark)

        (:name browse-kill-ring
               :description "Interactively insert items from kill ring"
               :type git
               :url "https://github.com/browse-kill-ring/browse-kill-ring.git"
               :features browse-kill-ring
               :prepare (progn
                  (autoload 'browse-kill-ring-default-keybindings "browse-kill-ring")))
        
        (:name easy-kill
               :description "Kill/Copy/Mark things easily"
               :type git
               :url "https://github.com/leoliu/easy-kill.git"
               :features easy-kill)
        
        (:name guide-key
               :description "Guide following keys to an input key sequence automatically and dynamically in Emacs."
               :type git
               :url "https://github.com/kbkbkbkb1/guide-key.git"
               :depends popwin
               :features guide-key)
        
        (:name ido
               :description "Interactively Do Things. Fuzzy match buffers/files/etc."
               :type http
               :url "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/ido.el")

        (:name ido-ubiquitous
               :description "Fancy completion all over Emacs, not just for buffers and files."
               :type git
               :url "https://github.com/DarwinAwardWinner/ido-ubiquitous"
               :depends ido
               :features ido-ubiquitous)

        (:name ido-vertical-mode
               :description "Makes ido-mode display vertically"
               :type git
               :url "https://github.com/gempesaw/ido-vertical-mode.el.git"
               :depends ido
               :features ido-vertical-mode)
        
        (:name kill-ring-search
               :description "Search the kill ring in the minibuffer."
               :type elpa)
        
        (:name fountain-mode
               :description "Major mode for editing screenplays in Fountain markup format"
               :type git
               :url "https://github.com/rnkn/fountain-mode.git"
               :features fountain-mode)
        
        (:name popwin
               :description "Popup Window Manager for Emacs"
               :type git
               :url "https://github.com/m2ym/popwin-el.git"
               :features popwin)

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

        (:name virtualenvwrapper
               :description "virtualenv tool for emacs python development"
               :type github
               :pkgname "porterjamesj/virtualenvwrapper.el"
               :features virtualenvwrapper)
        
        (:name wordsmith-mode
               :description "emacs package for syntax analysis"
               :type github
               :pkgname "istib/wordsmith-mode"
               :features wordsmith-mode)
        
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
         auto-complete
         dash
         diminish
         expand-region
         f
         jedi
         js2-mode
         js2-refactor
         magit
         markdown-mode
         multiple-cursors
         nxhtml
         org-mode
         psvn
         s
         skewer-mode
         smex
         yasnippet
         )
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

(provide 'init-packages)

