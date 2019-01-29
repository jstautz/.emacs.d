;;------------------------------------------------------------------------------
;;
;; Example org-mode config
;;
;;------------------------------------------------------------------------------

;; install org-mode... (the latest org-plus-contrib version)
(use-package org
  :ensure org-plus-contrib)

;; Standard org-mode setup
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook '(lambda()
                            (local-unset-key (kbd "C-c SPC"))))

;; Keybindings
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "<f5>") 'org-narrow-to-subtree)
;; I never want to accidentally turn on timestamp overlays.
(org-defkey org-mode-map "\C-c\C-x\C-t" nil)

;; Outline structure/style
(setq org-startup-indented t
      org-fontify-done-headline t
      org-blank-before-new-entry (quote ((heading) (plain-list-item)))
      org-tags-column 80
      org-cycle-separator-lines 1)

(diminish 'org-indent-mode)

;; Editing/Movement tweaks -- handling line navigation, etc.
(setq org-special-ctrl-a/e t
      org-catch-invisible-edits 'smart)

;; Don't warn me about executing shell scripts or elisp within links
;; WARNING: this is a security risk.
;; I've turned this off for the purposes of the demo, but I really recommend
;; leaving these turned on
(setq org-confirm-shell-link-function nil
      org-confirm-elisp-link-function nil
      org-confirm-babel-evaluate nil)

;; Additional org-mode-related packages

;; org-pomodoro -- nice little pomodoro timer in org-mode
(use-package org-pomodoro
  :config (setq org-pomodoro-finished-sound-args "-v 0.5"))
