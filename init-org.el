;;-----------------------------------------------------------------------------
;; My Org-mode configs are:
;; - Idiosyncratic
;; - Ugly
;; - Complicated
;; - Still need to be cleaned up. Here be dragons.
;;-----------------------------------------------------------------------------

(setq org-directory (concat home-dir "org/")
      writing-directory (concat home-dir "Documents/Writing/"))


;; Standard org-mode setup
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook 'turn-on-font-lock)


;; org modules to load
(setq org-modules (quote (org-bbdb
                          org-gnus
                          org-habit
                          org-info
                          org-jsinfo)))


;; highlight current line when in agenda
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;; My agenda setup (I like managing it here rather than thru custom)
(setq org-agenda-files '((concat org-dir "flagged.org")
                         (concat org-dir "inbox.txt")
                         (concat org-dir "projects.org")))


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

(add-hook 'org-mobile-pre-push-hook 'jcs:decrypt-secrets)
(add-hook 'org-mobile-pre-pull-hook 'jcs:decrypt-secrets)


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


;; UI / keys related
(setq org-level-color-stars-only t)
(setq org-odd-levels-only t)
(setq org-return-follows-link t)
(setq org-special-ctrl-a/e t)
(setq org-use-speed-commands t)
(setq org-M-RET-may-split-line t)
(setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))
(setq org-babel-no-eval-on-ctrl-c-ctrl-c t)
(setq org-completion-use-ido t)
(setq org-fast-tag-selection-single-key t)
(setq org-fontify-done-headline t)
(setq org-hide-leading-stars t)
(setq org-show-following-heading (quote ((default))))
(setq org-show-hierarchy-above (quote ((default . t) (tags-tree))))
(setq org-tags-column 80)
(setq org-show-notification-handler (quote (lambda (notification) (growl "org-mode notification" notification))))
(setq org-show-siblings (quote ((default) (isearch t))))

;; Agenda-related
(setq org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3)))
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-include-diary t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-unavailable-files t)
(setq org-agenda-sorting-strategy (quote ((agenda time-up priority-down) (todo priority-down) (tags priority-down))))
(setq org-agenda-span (quote week))
(setq org-agenda-start-with-clockreport-mode nil)
(setq org-agenda-start-with-follow-mode nil)
(setq org-agenda-tags-column -120)
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-todo-ignore-scheduled (quote future))
(setq org-agenda-todo-list-sublevels t)

;; Archiving
(setq org-archive-location (concat org-directory "archives.org::"))
(setq org-archive-mark-done nil)

;; Tags
(setq org-attach-auto-tag nil)

;; Refiling/inbox
(setq org-refile-targets (quote ((("~/org/projects.org" "~/org/someday_maybe.org") :maxlevel . 2))))
(setq org-reverse-note-order (quote (("inbox" . t))))

;; clocking
(setq org-clock-history-length 5)
(setq org-clock-idle-time 5)
(setq org-clock-in-resume t)
(setq org-clock-in-switch-to-state (quote bh/clock-in-to-started))
(setq org-clock-into-drawer t)
(setq org-clock-modeline-total (quote current))
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-persist t)
(setq org-clock-sound nil)

;; export
(setq org-combined-agenda-icalendar-file (concat org-directory "org.ics"))
(setq org-export-with-TeX-macros nil)

(setq org-confirm-shell-link-function (quote y-or-n-p))

;; to-dos
(setq org-default-priority 69)
(setq org-lowest-priority 69)
(setq org-priority-start-cycle-with-default t)
(setq org-enforce-todo-checkbox-dependencies nil)
(setq org-enforce-todo-dependencies nil)

(setq org-habit-show-habits-only-for-today nil)

(setq org-global-properties (quote (("Effort_ALL" . "0:05 0:10 0:15 0:25 1:00 2:00 4:00 8:00") ("Focus_ALL" . "High Medium Low"))))

(setq org-id-include-domain nil)
(setq org-id-method (quote uuidgen))

(setq org-log-done (quote time))
(setq org-log-into-drawer "LOGBOOK")
(setq org-log-redeadline (quote note))
(setq org-log-reschedule (quote note))


;; system
(setq org-file-apps (quote ((auto-mode . emacs)
                            ("\\.x?html?\\'" . default)
                            ("\\.pdf\\'" . default)
                            ("\\.celtx\\'" . system)
                            ("\\.doc\\'" . system)
                            ("\\.xls\\'" . system)
                            ("\\.fdr\\'" . system)
                            ("\\.dvi\\'" . system))))


;; calendar
(setq org-icalendar-combined-name "Org")
(setq org-icalendar-include-todo t)
(setq org-icalendar-store-UID t)

;; table
(setq org-table-export-default-format "orgtbl-to-csv")

(provide 'init-org)
