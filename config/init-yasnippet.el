(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory (concat dotemacs-dir "/snippets/"))
(yas-load-directory (concat dotemacs-dir "/el-get/feature-mode/snippets"))

;; Yasnippet's tab-completion should override org-mode's
(add-hook 'org-mode-hook 
          #'(lambda () 
              (local-set-key [tab] 'yas-expand)))

;; Also override markdown-mode's tab
(add-hook 'markdown-mode-hook 
          #'(lambda () 
              (local-set-key [tab] 'yas-expand)))
