;;; org-mode-init.el --- Org config -*- lexical-binding: t; -*-

;;; Setup
;; loaded at startup: the agenda feeds appointment reminders (see Diary & appointments)
(require 'org)
(defconst jcs:org-dir "~/org/")
(defconst jcs:writing-dir "~/Documents/Writing/")

;;; Drag and drop
;; dropping a file opens it instead of asking attach/open/link
(setopt org-yank-dnd-method 'open)

;;; org-mode-init.el ends here
