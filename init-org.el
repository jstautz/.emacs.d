;;-----------------------------------------------------------------------------
;; Org-mode -- This right here is the reason to use emacs
;;             (forgive the gnarly messy setup below -- lots of custom configs)
;;-----------------------------------------------------------------------------

(setq org-directory (concat home-dir "org/"))
(setq writing-directory (concat home-dir "Documents/Writing/"))

;; Standard org-mode setup
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; highlight current line when in agenda
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;; My agenda setup (I like managing it here rather than thru custom)
(setq org-agenda-files (quote ("~/org/flagged.org"
                               "~/org/inbox.txt"
                               "~/org/projects.org")))

;; Search on other files, too
(setq org-agenda-text-search-extra-files '("~/org/goals.org"
                                           "~/org/someday_maybe.org"
                                           "~/org/notes/tech_log.txt"
                                           "~/org/notes/reference.org"))

(setq org-stuck-projects (quote ("+LEVEL=2-REFILE-UNFILED-HABITS/-DONE"
  ("TODO" "NEXT" "STARTED") ("NOTES") "")))

(setq org-agenda-custom-commands
           '(
             (" " "Agenda overview"
              ((agenda"")
			   (tags "REFILE" 
					 ((org-agenda-overriding-header "Tasks to Refile")))
			   (org-agenda-list-stuck-projects) 
			   (tags-todo "-REFILE+Focus=\"\""
						  ((org-agenda-overriding-header "Tasks to Estimate")
						   (org-agenda-skip-function 'bh/skip-projects)
						   (org-tags-match-list-sublevels t)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   (tags-todo "-REFILE/!NEXT|STARTED"
			    		  ((org-agenda-overriding-header "Next Tasks")
			    		   (org-agenda-skip-function 'bh/skip-projects)
			    		   (org-agenda-todo-ignore-scheduled 'future)
			    		   (org-tags-match-list-sublevels t)
			    		   (org-agenda-sorting-strategy
			    			'(todo-state-down effort-up category-keep))))
			   (tags-todo "@work-REFILE"
						  ((org-agenda-overriding-header "Work Tasks")
						   (org-tags-match-list-sublevels t)
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   (tags-todo "@home-REFILE"
						  ((org-agenda-overriding-header "Home Tasks")
						   (org-tags-match-list-sublevels t)
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   (tags-todo "@desk-REFILE"
						  ((org-agenda-overriding-header "Writing/Focused Tasks")
						   (org-tags-match-list-sublevels t)
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   (tags-todo "@cpu-REFILE"
						  ((org-agenda-overriding-header "Computer Tasks")
						   (org-tags-match-list-sublevels t)
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   (tags-todo "@errands-REFILE"
						  ((org-agenda-overriding-header "Errands")
						   (org-tags-match-list-sublevels t)
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   (tags-todo "@transit-REFILE"
						  ((org-agenda-overriding-header "Transit Tasks")
						   (org-tags-match-list-sublevels t)
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   ))
			 ("c" "@cpu + calendar"
              ((agenda "")
			   (tags-todo "@cpu-REFILE" 
						  ((org-agenda-overriding-header "All Computer Tasks")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
               ))
			 ("d" "@desk"
              ((tags-todo "@desk-REFILE" 
						  ((org-agenda-overriding-header "All Desk/Writing Tasks")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
              ))
             ("h" "@home + calendar"
              ((agenda "")
			   (tags-todo "@home-REFILE" 
						  ((org-agenda-overriding-header "All Home Tasks")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
			   ))
             ("r" "@errands" tags-todo "@errands")
             ("z" "@transit" tags-todo "@transit")
             ("w" "@work + agenda"
              ((agenda "")
			   (tags-todo "@work-REFILE" 
						  ((org-agenda-overriding-header "All Work Tasks")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))

			   ))
			 ))

;; A couple helper functions for org agendas from Bernt Hansen
;; http://doc.norang.ca/org-mode.html

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (let ((has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t)))
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (member (org-get-todo-state) org-todo-keywords-1)
          (setq has-subtask t))))
    (and is-a-task has-subtask)))

(defun bh/skip-projects ()
  "Skip trees that are projects"
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (cond
     ((bh/is-project-p)
      next-headline)
     (t
      nil))))

;; Resume org clocks if I restart emacs w/ running clock
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; Change task to STARTED when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-started)

(defun bh/clock-in-to-started (kw)
  "Switch task from TODO or NEXT to STARTED when clocking in.
Skips capture tasks."
  (if (and (member (org-get-todo-state) (list "TODO" "NEXT"))
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      "STARTED"))

;; Get rid of empty clock drawers
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "CLOCK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


;; using remember with org-mode (deprecated, but I like it more than org-capture)
(org-remember-insinuate)
(setq org-default-notes-file (concat org-directory "inbox.txt"))
(define-key global-map "\C-cr" 'org-remember)

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; remember templates for org-remember:
;;   t = new TODO item
;;   j = new journal entry (dated today)
;;   i = new inbox item
;;   q = only used via QuickSilver -- new inbox item
     (setq org-remember-templates
      '(("Todo" ?t "*** TODO %^{Action to be taken} %^G\n    %?\n    Added: %U" (concat org-directory "projects.org") "Unfiled")
        ("Journal" ?j "* %U %^{Title} %^g\n\n%?\n" (concat writing-directory "01-composting/journal.txt") top)
        ("Inbox" ?i "* %U %^{Title}\n  %i%?\n   %a\n\n" (concat org-directory "inbox.txt") top)
        ("oldQS remember" ?f "* %U %?\n%i\n%a\n\n" (concat org-directory "inbox.txt") top)
        ("QS remember" ?q "* %i\n\n  Added: %U" (concat org-directory "inbox.txt") top)
        ("AppleScript remember" ?y "* %:shortdesc\n  %:initial\n   Source: %u, %c\n\n  %?" (concat org-directory "inbox.txt"))
        ("AppleScript note" ?z "* %?\n\n  Date: %u\n" (concat org-directory "inbox.org"))))

(require 'org-annotation-quicksilver)

;; org-mac-protocol is replacing oaq (eh, haven't got this to work yet)
;;(require 'org-mac-protocol)

;; trying org-capture instead of remember -- haven't really got too solidly into this yet.
(setq org-capture-templates
	  '(("t" "Todo" entry
		 (file+headline (concat org-directory "projects.org") "Unfiled")
		 "*** TODO %^{Action to be taken} %^G\n    %?\n    Added: %U" :prepend t)
		("j" "Journal" entry
		 (file (concat writing-directory "01-composting/journal.txt"))
		 "* %U %^{Title} %^g\n\n%?\n" :prepend t)
		("i" "Inbox" entry
		 (file (concat org-directory "inbox.txt"))
		 "* %U %^{Title}\n  %i%?\n   %a\n\n" :prepend t)
		("q" "QS remember" entry
		 (file (concat org-directory "inbox.txt"))
		 "* %U %?\n%i\n%a\n\n" :prepend t)
		("y" "AppleScript remember" entry
		 (file+headline
		  (concat org-directory "inbox.txt")
		  "")
		 "* %:shortdesc\n  %:initial\n   Source: %u, %c\n\n  %?" :prepend t)
		("z" "AppleScript note" entry
		 (file+headline
		  (concat org-directory "inbox.txt")
		  "")
		 "* %?\n\n  Date: %u\n" :prepend t)
		("q" "QS Inbox" entry
		 (file+headline
		  (concat org-directory "inbox.txt")
		  "Inbox")
		 "\n* QS %U%?\n%i\n%a" :prepend t)))

;; remind me of any appointments
(appt-activate 1) 


;; org-mobile settings
(setq org-mobile-files '("~/org/projects.org""~/org/inbox.txt" "~/org/notes/book_list.org" "~/org/goals.org" "~/org/someday_maybe.org"))
(setq org-mobile-inbox-for-pull (concat org-directory "flagged.org"))
(setq org-mobile-directory (concat home-dir "Dropbox/MobileOrg"))
(setq org-mobile-use-encryption t)
;; I don't care about possible leakage in autosave files
(setq org-crypt-disable-auto-save nil)

(defun jcs:org-mobile-decrypt-creds ()
  (interactive)
  (require 'secrets))

(add-hook 'org-mobile-pre-push-hook 'jcs:org-mobile-decrypt-creds)
(add-hook 'org-mobile-pre-pull-hook 'jcs:org-mobile-decrypt-creds)


;; org keybindings
(global-set-key (kbd "<f5>") 'org-narrow-to-subtree)
(global-set-key (kbd "<M-f5>") 'jcs:org-todo-tree)
(global-set-key (kbd "<S-f5>") 'jcs:widen)
(global-set-key (kbd "<f6>") 'org-clock-goto)

(defun jcs:org-todo-tree ()
  (interactive)
  (org-narrow-to-subtree)
  (org-show-todo-tree nil))

(defun jcs:widen ()
  (interactive)
  (widen)
  (org-reveal)
  (org-remove-occur-highlights))


;;-----------------------------------------------------------------------------
;; Diary and appt settings (integrates with org-mode)
;;-----------------------------------------------------------------------------
(setq diary-file (concat org-directory "calendar.diary"))
(add-hook 'diary-display-hook 'fancy-diary-display)
(setq list-diary-entries-hook
      '(include-other-diary-files sort-diary-entries))
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)

(require 'appt)
(setq org-agenda-include-diary t)
(setq appt-time-msg-list nil)
(org-agenda-to-appt)

;; Re-load agenda dates/items into appt whenever I load agenda view
(defadvice  org-agenda-redo (after org-agenda-redo-add-appts)
  "Pressing `r' on the agenda will also add appointments."
  (progn 
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt)))

(ad-activate 'org-agenda-redo)

;; Reset the appointments every day at one minute after midnight
(run-at-time "24:01" 86400 'org-agenda-redo)

;; Set Agenda view to show Habits again each day at 4am
(run-at-time "04:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(provide 'init-org)
