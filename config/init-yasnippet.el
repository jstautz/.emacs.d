(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory (concat dotemacs-dir "/snippets/"))
(yas-load-directory (concat dotemacs-dir "/el-get/feature-mode/snippets"))

;; Override markdown-mode's tab
(add-hook 'markdown-mode-hook 
          #'(lambda () 
              (local-set-key [tab] 'yas-expand)))
