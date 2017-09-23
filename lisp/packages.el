;;------------------------------------------------------------------------------
;;
;; Install non-org-mode packages I use in this demo
;;
;;------------------------------------------------------------------------------



;; Ace Jump Mode
(use-package ace-jump-mode
             :bind ("C-." . ace-jump-mode))

;; Browse Kill Ring
(use-package browse-kill-ring
             :defer t
             :init
             (progn
               (autoload 'browse-kill-ring-default-keybindings "browse-kill-ring")
               (browse-kill-ring-default-keybindings)))

;; Diminish mode
(use-package diminish)

;; Ido mode (lots of settings here...)
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-everywhere t
      completion-ignore-case t           
      read-file-name-completion-ignore-case t
      ido-max-prospects 20
      ido-use-faces t)
(setq ido-record-ftp-work-directories nil
      ido-enable-tramp-completion nil
      ido-is-tramp-root nil)
(recentf-mode t)
(setq ido-use-virtual-buffers t)
(setq ido-auto-merge-work-directories-length -1
      ido-show-dot-for-dired t
      ido-use-filename-at-point 'guess)
(put 'ido-exit-minibuffer 'disabled nil)

;; Ido mode: ubiquitous and vertical mode
(use-package ido-completing-read+)
(ido-ubiquitous-mode 1)
(use-package ido-vertical-mode
             :init
             (progn (ido-vertical-mode 1)
                    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))

;; Guide key
(use-package guide-key)
(defun guide-key/jcs-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x"))
(add-hook 'org-mode-hook 'guide-key/jcs-hook-function-for-org-mode)
(setq guide-key/idle-delay 1)
(setq guide-key/popup-window-position 'bottom)
(guide-key-mode 1)
(diminish 'guide-key-mode)

;; Magit!
(use-package magit)
(setq magit-push-always-verify nil)

;; Pop-win: Short-lived pop-up windows
(use-package popwin)
(defun popwin-bkr:update-window-reference ()
  (popwin:update-window-reference 'browse-kill-ring-original-window :safe t))
(add-hook 'popwin:after-popup-hook 'popwin-bkr:update-window-reference)
(push "*Kill Ring*" popwin:special-display-config)
(popwin-mode 1)

;; Smexy smex
(use-package smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
