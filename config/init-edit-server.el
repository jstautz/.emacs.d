;; Raise emacs function
(defun ns-raise-emacs ()
  (ns-do-applescript "tell application \"Emacs\" to activate"))

;; Back to Chrome function -- send me back after I'm done editing
(defun ns-raise-chrome ()
  (ns-do-applescript "tell application \"Chrome\" to activate"))

(add-hook 'server-visit-hook 'ns-raise-emacs)
(add-hook 'edit-server-start-hook 'ns-raise-emacs)
(add-hook 'edit-server-done-hook 'ns-raise-chrome)

;; turn off auto-fill, turn on wrap lines, make the frame larger
(add-hook 'edit-server-start-hook 'turn-off-auto-fill)
(add-hook 'edit-server-start-hook 'turn-on-visual-line-mode)

(setq edit-server-new-frame nil)
(edit-server-start)
