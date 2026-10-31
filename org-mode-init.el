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

;;; org-mode-init.el ends here
