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
              ("M-d" . term-send-forward-kill-word-partial))))

;;-----------------------------------------------------------------------------
;; Fix yanking into Emacs terminal
;; Mad props to Brian Zwahr
;; http://emacs-journey.blogspot.ca/2012/06/improving-ansi-term.html?m=1
;;-----------------------------------------------------------------------------

(defun my-term-paste (&optional string)
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (if string string (current-kill 0))))

