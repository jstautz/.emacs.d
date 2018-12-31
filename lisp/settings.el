;;------------------------------------------------------------------------------
;;
;; Interface and editor options I like
;;
;;------------------------------------------------------------------------------

;; Get rid of all that chrome and fuss

;; Don't talk to me

;; Don't let me quit on accident

;; I'm a Mac

;; Show me where I am

;; Word wrap and center my text nicely

;; Don't scroll like a maniac

;; Respect the power of my mouse wheel, margins!

;; Scroll around the cursor

;; Select text in a non-psychopathic way


;; Enable rectangle editing, but not all the other CUA hoo-hah

;; Preserve indentation
(global-set-key (kbd "RET") 'newline-and-indent)

;; Trust me, I'm an interior decorator

;; Don't crap up my working directory with backups

;; I'm paranoid

;; Unset some defaults I don't like

;; Forgive my muscle memory

;; Escape is my eject button
(global-set-key (kbd "<C-escape>") 'top-level)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(add-hook 'org-mode-hook
          (lambda()
            (define-key org-mode-map (kbd "<escape>") 'keyboard-escape-quit)))

;; I love ibuffer

;; I also love ispell

;; And also, I'm Canadian.

;; Save my place


;; Take off some guard rails
(put 'narrow-to-region 'disabled nil)

;; Let me jump between a split frame and single-window view

;; Diminish some things
(diminish 'auto-revert-mode)

;; Send my notifications using terminal-notifier

