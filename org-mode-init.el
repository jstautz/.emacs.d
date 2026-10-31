;;; org-mode-init.el --- Org config -*- lexical-binding: t; -*-

;;; Setup
;; loaded at startup: the agenda feeds appointment reminders (see Diary & appointments)
(require 'org)
(defconst jcs:org-dir "~/org/")
(defconst jcs:writing-dir "~/Documents/Writing/")

;;; Drag and drop
;; dropping a file opens it instead of asking attach/open/link
(setopt org-yank-dnd-method 'open)

;;; Navigation
(defun jcs:org-todo-tree ()
  "Narrow to this subtree and show only its TODOs."
  (interactive)
  (org-narrow-to-subtree)
  (org-show-todo-tree nil))

(defun jcs:widen ()
  "Widen, re-show the current entry, and clear sparse-tree highlights."
  (interactive)
  (widen)
  (org-reveal)
  (org-remove-occur-highlights))

;; speed key S widens (speed keys work at the start of a headline)
(add-to-list 'org-speed-commands '("S" . jcs:widen))

;;; Keys
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "<f5>" #'org-narrow-to-subtree)
(keymap-global-set "M-<f5>" #'jcs:org-todo-tree)
(keymap-global-set "S-<f5>" #'jcs:widen)
;; same as S-<f5>, for keyboards where Fn/Ctrl get mixed up
(keymap-global-set "C-S-<f5>" #'jcs:widen)
(keymap-global-set "<f6>" #'org-clock-goto)

;; never toggle timestamp overlays by accident
(keymap-unset org-mode-map "C-c C-x C-t")

;;; Outline style
(setq org-startup-indented t
      org-level-color-stars-only t
      org-fontify-done-headline t
      org-blank-before-new-entry '((heading) (plain-list-item))
      org-tags-column 80
      org-cycle-separator-lines 0)

;;; Editing
;; speed keys: single-letter commands at the start of a headline (? lists them)
(setq org-use-speed-commands t
      org-fast-tag-selection-single-key t)

;; line motion, links, code blocks
(setq org-special-ctrl-a/e t
      org-M-RET-may-split-line t
      org-return-follows-link t
      org-babel-no-eval-on-ctrl-c-ctrl-c t
      org-link-shell-confirm-function #'y-or-n-p
      org-fold-catch-invisible-edits 'smart)

;; hide *bold* /italic/ markers (pretty entities made filenames with _ messy)
(setq org-hide-emphasis-markers t)

;;; org-mode-init.el ends here
