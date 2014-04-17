(defun guide-key/jcs-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x"))
(add-hook 'org-mode-hook 'guide-key/jcs-hook-function-for-org-mode)

(setq guide-key/idle-delay 1)

(setq guide-key/popup-window-position 'bottom)

(guide-key-mode 1)
