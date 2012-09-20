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
