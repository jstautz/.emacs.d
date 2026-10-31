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

;;; Links & attachments
;; files Org opens outside Emacs
(setq org-file-apps '((auto-mode . emacs)
                      ("\\.x?html?\\'" . default)
                      ("\\.pdf\\'" . default)
                      ("\\.docx?\\'" . system)
                      ("\\.xls\\'" . system)
                      ("\\.dvi\\'" . system)))

;; tag attachments so they're findable; children inherit the parent's attach dir
(setopt org-attach-auto-tag "attachment"
        org-attach-use-inheritance t
        org-attach-method 'mv)

;;; Agenda
(require 'org-agenda)
(setq org-agenda-files (list (concat jcs:org-dir "inbox.org")
                             (concat jcs:org-dir "todo.org"))
      ;; also searched by C-c a s (text search)
      org-agenda-text-search-extra-files
      (mapcar (lambda (file) (concat jcs:org-dir file))
              '("goals.org" "someday_maybe.org" "gift_ideas.org"
                "tech_log.txt" "read_listen_watch.org")))

(setq org-agenda-dim-blocked-tasks t
      org-agenda-tags-column 80)

;; default views and sorting
(setq org-agenda-include-diary t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-unavailable-files t
      org-agenda-sorting-strategy '((agenda todo-state-down time-up priority-down)
                                    (todo todo-state-down priority-down)
                                    (tags todo-state-down priority-down))
      org-agenda-span 'week)

;; TODO lists: hide future-scheduled items, include sublevels
(setq org-agenda-tags-todo-honor-ignore-options nil
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-todo-list-sublevels t
      org-tags-match-list-sublevels t
      org-agenda-start-with-clockreport-mode nil
      org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3)
      ;; stuck = a level-2 project (not inbox, habits or goals) with no next action
      org-stuck-projects
      '("+LEVEL=2-REFILE-UNFILED-HABITS-GOALS-goal_month-goal_quarter-goal_year/-DONE"
        ("NEXT" "STARTED" "TODO") ("NOTES") ""))

;; reusable agenda blocks, combined into custom views below
(defvar jcs:org-agenda-block--purpose-header
  '(tags "TOPOFAGENDA"
         ((org-agenda-overriding-header "\nI am the sunlight that ignites creative transformation\n")
          (org-tags-match-list-sublevels nil)))
  "Top of Agenda.")

(defvar jcs:org-agenda-block--today-schedule
  '(agenda "" ((org-agenda-overriding-header "Today's Schedule:")
               (org-agenda-span 'day)
               (org-agenda-start-on-weekday nil)
               (org-agenda-start-day "+0d")))
  "A block showing a 1 day schedule.")

(defvar jcs:org-agenda-block--upcoming-week
  '(agenda "" ((org-agenda-overriding-header "Next 7 Days:")
               (org-agenda-span 7)
               (org-agenda-start-on-weekday nil)
               (org-agenda-start-day "+0d")))
  "A block showing my schedule for the next 7 days.")

(defvar jcs:org-agenda-block--inbox
  '(tags "REFILE"
         ((org-agenda-overriding-header "Inbox items to process / refile:")))
  "Items from my inbox and/or needing refiling.")

(defvar jcs:org-agenda-block--estimate
  '(tags-todo "+Effort=\"\""
              ((org-agenda-overriding-header "Tasks to estimate:")))
  "Tasks I need to add estimates to.")

(defvar jcs:org-agenda-block--goals-year
  '(tags "goal_year"
         ((org-agenda-overriding-header "Yearly OKRs:")))
  "Yearly Goals")

(defvar jcs:org-agenda-block--goals-quarter
  '(tags "goal_quarter"
         ((org-agenda-overriding-header "Quarterly OKRs:")))
  "Quarterly Goals")

(defvar jcs:org-agenda-block--goals-month
  '(tags "goal_month"
         ((org-agenda-overriding-header "Monthly Goals:")))
  "Monthly Goals")

(defvar jcs:org-agenda-block--goals-week
  '(tags "goal_week"
         ((org-agenda-overriding-header "Weekly Goals:")))
  "Weekly Goals")

(defvar jcs:org-agenda-block--tasks-today
  '(tags-todo "+PRIORITY=\"A\""
              ((org-agenda-overriding-header "Today's Priorities:")))
  "Tasks for Today (A priority tasks)")

(defvar jcs:org-agenda-block--tasks-next
  '(todo "NEXT"
         ((org-agenda-overriding-header "All Next Actions (except future scheduled):")
          (org-agenda-todo-ignore-scheduled 'future)))
  "All Next Actions, except those with scheduled dates set in the future")

(defvar jcs:org-agenda-block--next-home
  '(tags-todo "home-REFILE/!NEXT|STARTED"
              ((org-agenda-overriding-header "Home Next/Started Actions -- Not scheduled")
               (org-agenda-todo-ignore-scheduled 'future)
               (org-agenda-tags-todo-honor-ignore-options t)))
  "Home next actions, except future-scheduled ones.")

(defvar jcs:org-agenda-block--todo-home
  '(tags-todo "home-REFILE/!TODO|WAITING"
              ((org-agenda-overriding-header "Home Backlog -- Not scheduled")
               (org-agenda-todo-ignore-scheduled 'future)
               (org-agenda-tags-todo-honor-ignore-options t)))
  "Home backlog, except future-scheduled ones.")

(defvar jcs:org-agenda-block--next-work
  '(tags-todo "work-REFILE/!NEXT|STARTED"
              ((org-agenda-overriding-header "Work Next/Started Actions -- Not scheduled")
               (org-agenda-todo-ignore-scheduled 'future)
               (org-agenda-tags-todo-honor-ignore-options t)))
  "Work next actions, except future-scheduled ones.")

(defvar jcs:org-agenda-block--todo-work
  '(tags-todo "work-REFILE/!TODO|WAITING"
              ((org-agenda-overriding-header "Work Backlog -- Not scheduled")
               (org-agenda-todo-ignore-scheduled 'future)
               (org-agenda-tags-todo-honor-ignore-options t)))
  "Work backlog, except future-scheduled ones.")

(defvar jcs:org-agenda-block--next-coaching
  '(tags-todo "coaching-REFILE/!NEXT|STARTED"
              ((org-agenda-overriding-header "Coaching Next/Started Actions -- Not scheduled")
               (org-agenda-todo-ignore-scheduled 'future)
               (org-agenda-tags-todo-honor-ignore-options t)))
  "Coaching next actions, except future-scheduled ones.")

(defvar jcs:org-agenda-block--todo-coaching
  '(tags-todo "coaching-REFILE/!TODO|WAITING"
              ((org-agenda-overriding-header "Coaching Backlog -- Not scheduled")
               (org-agenda-todo-ignore-scheduled 'future)
               (org-agenda-tags-todo-honor-ignore-options t)))
  "Coaching backlog, except future-scheduled ones.")

(defvar jcs:org-agenda-block--next-writing
  '(tags-todo "writing-REFILE/!NEXT|STARTED"
              ((org-agenda-overriding-header "Writing Next/Started Actions -- Not scheduled")
               (org-agenda-todo-ignore-scheduled 'future)
               (org-agenda-tags-todo-honor-ignore-options t)))
  "Writing next actions, except future-scheduled ones.")

(defvar jcs:org-agenda-block--todo-writing
  '(tags-todo "writing-REFILE/!TODO|WAITING"
              ((org-agenda-overriding-header "Writing Backlog -- Not scheduled")
               (org-agenda-todo-ignore-scheduled 'future)
               (org-agenda-tags-todo-honor-ignore-options t)))
  "Writing backlog, except future-scheduled ones.")

(defvar jcs:org-agenda-block--next-writing-mgr
  '(tags-todo "writing_mgr-REFILE/!NEXT|STARTED"
              ((org-agenda-overriding-header "Writing Manager Next/Started Actions -- Not scheduled")
               (org-agenda-todo-ignore-scheduled 'future)
               (org-agenda-tags-todo-honor-ignore-options t)))
  "Writing_Mgr next actions, except future-scheduled ones.")

(defvar jcs:org-agenda-block--todo-writing-mgr
  '(tags-todo "writing_mgr-REFILE/!TODO|WAITING"
              ((org-agenda-overriding-header "Writing Manager Backlog -- Not scheduled")
               (org-agenda-todo-ignore-scheduled 'future)
               (org-agenda-tags-todo-honor-ignore-options t)))
  "Writing_Mgr backlog, except future-scheduled ones.")

(defvar jcs:org-agenda-block--today-tasks
  '(tags-todo "+PRIORITY=\"A\"|+PRIORITY=\"B\""
              ((org-agenda-overriding-header "Today's tasks\n------------")
               (org-agenda-todo-ignore-scheduled 'future)
               (org-agenda-tags-todo-honor-ignore-options t)))
  "Priority A and B tasks are tasks for Today.")


;; custom views: C-c a SPC overview, w work, h home, G goals, T today, ...
(setq org-agenda-custom-commands
      `((" " "Overview"
         (,jcs:org-agenda-block--purpose-header
          ,jcs:org-agenda-block--upcoming-week
          ,jcs:org-agenda-block--inbox
          (org-agenda-list-stuck-projects)
          ,jcs:org-agenda-block--estimate
          ,jcs:org-agenda-block--goals-year
          ,jcs:org-agenda-block--goals-quarter
          ,jcs:org-agenda-block--goals-month
          ,jcs:org-agenda-block--goals-week
          ,jcs:org-agenda-block--tasks-today
          ,jcs:org-agenda-block--tasks-next))

        ("G" "Goals"
         (,jcs:org-agenda-block--purpose-header
          ,jcs:org-agenda-block--goals-week
          ,jcs:org-agenda-block--goals-month
          ,jcs:org-agenda-block--goals-quarter
          ,jcs:org-agenda-block--goals-year))

        ("W" "Weekly Goals"
         (,jcs:org-agenda-block--purpose-header
          ,jcs:org-agenda-block--goals-week))

        ("h" "Home Tasks + Agenda"
         (,jcs:org-agenda-block--purpose-header
          ,jcs:org-agenda-block--upcoming-week
          ,jcs:org-agenda-block--next-home
          ,jcs:org-agenda-block--todo-home))

        ("w" "Work Tasks + Agenda"
         (,jcs:org-agenda-block--purpose-header
          ,jcs:org-agenda-block--upcoming-week
          ,jcs:org-agenda-block--next-work
          ,jcs:org-agenda-block--todo-work))

        ("c" "Coaching Tasks + Agenda"
         (,jcs:org-agenda-block--purpose-header
          ,jcs:org-agenda-block--upcoming-week
          ,jcs:org-agenda-block--next-coaching
          ,jcs:org-agenda-block--todo-coaching))

        ("r" "Writing Tasks"
         (,jcs:org-agenda-block--purpose-header
          ,jcs:org-agenda-block--next-writing
          ,jcs:org-agenda-block--todo-writing))

        ("g" "Writing Manager Tasks"
         (,jcs:org-agenda-block--purpose-header
          ,jcs:org-agenda-block--next-writing-mgr
          ,jcs:org-agenda-block--todo-writing-mgr))

        ("e" "errands + phone" tags-todo "errands|phone")

        ("R" "reading" tags-todo "reading")

        ("L" "listen/watch" tags-todo "listen_watch")

        ("D" "Daily review"
         agenda ""
         ((org-agenda-span 'day)
          (org-agenda-start-on-weekday 0)
          (org-agenda-start-with-log-mode t)
          (org-agenda-use-time-grid nil)
          (org-agenda-toggle-diary nil)
          (org-agenda-skip-function
           '(org-agenda-skip-entry-if 'nottodo 'done))))

        ("T" "TODAY A or B Priority tasks"
         (,jcs:org-agenda-block--purpose-header
          ,jcs:org-agenda-block--today-schedule
          ,jcs:org-agenda-block--today-tasks))))

;;; Diary & appointments
(setopt diary-file (concat jcs:org-dir "calendar.diary"))
;; follow #include lines in the diary, and sort entries
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

;; load today's timed agenda items as appointment reminders
(require 'appt)
(defun jcs:org-agenda-refresh-appts (&rest _)
  "Rebuild the appointment list from the agenda."
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))
(jcs:org-agenda-refresh-appts)
;; r in the agenda also refreshes reminders
(advice-add 'org-agenda-redo :after #'jcs:org-agenda-refresh-appts)
;; and again just after midnight ("24:01" = 00:01 tomorrow)
(run-at-time "24:01" 86400 #'jcs:org-agenda-refresh-appts)

;;; Tasks
;; priorities run A-E; new tasks default to E
(setq org-priority-default ?E
      org-priority-lowest ?E
      org-priority-start-cycle-with-default t
      org-enforce-todo-dependencies nil
      org-enforce-todo-checkbox-dependencies nil
      org-id-include-domain nil
      org-id-method 'uuid)

;; habits: repeating tasks with a consistency graph in the agenda
(require 'org-habit)
(setq org-habit-show-habits-only-for-today nil
      org-habit-show-all-today t)
;; habits hidden with K in the agenda come back each morning
(defun jcs:org-show-habits-again ()
  "Show habits in the agenda again."
  (setq org-habit-show-habits t))
(run-at-time "04:00" 86400 #'jcs:org-show-habits-again)

;;; org-mode-init.el ends here
