;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; My Org-mode configs
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Relevant directories
    (setq org-dir (concat home-dir "org/")
          writing-dir (concat home-dir "Documents/Writing/"))

    ;; Standard org-mode setup
    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
    (add-hook 'org-mode-hook 'turn-on-font-lock)
    (add-hook 'org-mode-hook #'(lambda()
                                (local-unset-key (kbd "C-c SPC"))))


    ;;-----------------------------------------------------------------------------
    ;; Org interface tweaks
    ;;-----------------------------------------------------------------------------

    ;; Keybindings
    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (global-set-key (kbd "<f5>") 'org-narrow-to-subtree)
    (global-set-key (kbd "<M-f5>") 'jcs:org-todo-tree)
    (global-set-key (kbd "<S-f5>") 'jcs:widen)
    (global-set-key (kbd "<f6>") 'org-clock-goto)
    ;; Because typos. When switching between laptop keyboard and USB keyboard
    (global-set-key (kbd "<C-S-f5>") 'jcs:widen)

    ;; I never want to accidentally turn on timestamp overlays.
    (org-defkey org-mode-map "\C-c\C-x\C-t" nil)

    (org-defkey org-mode-map "\C-a" 'org-beginning-of-line)
    (org-defkey org-mode-map "\C-e" 'org-end-of-line)
    (org-defkey org-mode-map "\C-k" 'org-kill-line)

    ;; Outline structure/style
    (setq org-startup-indented t
          org-odd-levels-only nil
          org-hide-leading-stars nil
          org-level-color-stars-only t
          org-fontify-done-headline t
          org-blank-before-new-entry (quote ((heading) (plain-list-item)))
          org-tags-column 80
          org-cycle-separator-lines 0)

    ;; Editing/Movement tweaks -- turn on speed commands, fast tags, and ido
    (setq org-use-speed-commands t
          org-completion-use-ido t
          org-fast-tag-selection-single-key t)

    ;; Editing/Movement tweaks -- handling line navigation, links, code blocks
    (setq org-special-ctrl-a/e t
          org-M-RET-may-split-line t
          org-return-follows-link t
          org-babel-no-eval-on-ctrl-c-ctrl-c t
          org-confirm-shell-link-function (quote y-or-n-p)
          org-catch-invisible-edits 'smart)

    ;; Let org know how to open links to certain file types if not in Emacs
    (setq org-file-apps (quote ((auto-mode . emacs)
                                ("\\.x?html?\\'" . default)
                                ("\\.pdf\\'" . default)
                                ("\\.celtx\\'" . system)
                                ("\\.doc\\'" . system)
                                ("\\.xls\\'" . system)
                                ("\\.fdr\\'" . system)
                                ("\\.dvi\\'" . system))))

    ;; Show some context when digging into tags-trees / searches
    (setq org-show-following-heading (quote ((default)))
          org-show-hierarchy-above (quote ((default . t) (tags-tree)))
          org-show-siblings (quote ((default) (isearch t))))

    ;; Add tags when attaching, so's I can find attachments later
    (setq org-attach-auto-tag "attachment")

    ;; A couple custom navigation functions
    (defun jcs:org-todo-tree ()
      (interactive)
      (org-narrow-to-subtree)
      (org-show-todo-tree nil))

    (defun jcs:widen ()
      (interactive)
      (widen)
      (org-reveal)
      (org-remove-occur-highlights))

    (add-to-list 'org-speed-commands (cons "S" 'jcs:widen))

    ;;-----------------------------------------------------------------------------
    ;; Agenda setup
    ;;-----------------------------------------------------------------------------
    (setq org-agenda-files '("~/org/inbox.org"
                             "~/org/todo.org"))

    ;; TODO: why can't I replace these with (concat org-dir "filename.org")?
    ;;  gives me "Wrong type argument: stringp, (concat org-dir "flagged.org")"

    ;; Search on other files, too
    (setq org-agenda-text-search-extra-files '("~/org/goals.org"
                                               "~/org/someday_maybe.org"
                                               "~/org/notes/gift_ideas.org"
                                               "~/org/notes/tech_log.txt"
                                               "~/org/notes/reference.org"))

    ;; Agenda interface tweaks
    (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
    (setq org-agenda-dim-blocked-tasks t
          org-agenda-tags-column 80
          org-agenda-start-with-follow-mode nil
          org-agenda-compact-blocks nil)


    ;; Default agenda views & sorting
    (setq org-agenda-include-diary t
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-unavailable-files t
          org-agenda-sorting-strategy (quote ((agenda todo-state-down time-up priority-down) (todo todo-state-down priority-down) (tags todo-state-down priority-down)))
          org-agenda-span (quote day))

    ;; Agenda TODO options
    (setq org-agenda-tags-todo-honor-ignore-options nil
          org-agenda-todo-ignore-scheduled (quote future)
          org-agenda-todo-list-sublevels t)

    ;; Options for clock reports in agenda
    (setq org-agenda-start-with-clockreport-mode nil
          org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3)))

    ;; Definition of a "stuck project" for agenda
    (setq org-stuck-projects (quote ("+LEVEL=1-REFILE-UNFILED-HABITS/-DONE"
      ("TODO" "NEXT" "STARTED") ("NOTES") "")))

    ;; Custom function to return agenda header strings based on WIP limit
    (defun jcs:wip-text (tags todo limit)
      "Return string to indicate whether WIP limit is exceeded for a particular
    tag/todo keyword/limit. For use in agenda-overriding-header functions.
    If limit exceeded, string returned is wrapped in #s"
      (cond
       ((equal "TODO" todo)
        (setq org-wip-header-text "Queue"))
       ((equal "STARTED" todo)
        (setq org-wip-header-text "Doing"))
       ((equal "NEXT" todo)
        (setq org-wip-header-text "To Do Today"))
       ((equal "WAITING" "WAITING")
        (setq org-wip-header-text "Impeded / Waiting Response"))
       (t
        (setq org-wip-header-text "Queue"))
       )
      (if (<= (length (org-map-entries t (concat tags "/+" todo) 'agenda)) limit)
          org-wip-header-text
        (concat "### " org-wip-header-text  " -- over WIP limit (" (int-to-string limit) ") ###")))



    ;; My custom Agenda commands

(setq org-agenda-custom-commands
      '(("y" "Yearly OKRs"
         tags "goal_year" 
         ((org-agenda-overriding-header "\nI am the sunlight that ignites creative transformation\n\nYearly OKRs\n------------")))
        ("q" "Quarterly OKRs"
         tags "goal_quarter"
         ((org-agenda-overriding-header "\nI am the sunlight that ignites creative transformation\n\nQuarterly OKRs\n------------")))
        ("M" "Monthly Goals"
         tags "goal_month"
         ((org-agenda-overriding-header "\nI am the sunlight that ignites creative transformation\n\nMonthly Goals\n------------")))
        ("w" "Weekly Goals"
         tags "goal_week"
         ((org-agenda-overriding-header "\nI am the sunlight that ignites creative transformation\n\nMake results a weekly habit.\nThis is the core of accountability.\n\nWeekly Goals\n------------")))

        ("e" "To Estimates"
         tags-todo "-goal_week-goal_month-goal_quarter-goal_year+Effort=\"\"|-goal_week-goal_month-goal_quarter-goal_year-lowenergy-focused-mediumenergy"
         ((org-agenda-overriding-header "To Estimate\n------------")))

        ("o" "Other (non-MIT) tasks"
         tags-todo "-goal_week-goal_month-goal_quarter-goal_year-PRIORITY=\"A\""
         ((org-agenda-overriding-header "Non-MIT tasks\n------------")))

        ("T" "TODAY A or B Priority tasks"
         tags-todo "+PRIORITY=\"A\"|+PRIORITY=\"B\""
         ((org-agenda-overriding-header "Today's tasks\n------------")))

        )
      )



  ;; Old custom Agenda commands

  ;; (setq org-agenda-custom-commands
  ;;            '(
  ;;              (" " "Agenda overview"
  ;;               ((agenda"")
  ;;                (tags "REFILE" 
  ;;                      ((org-agenda-overriding-header "Tasks to Refile")))
  ;;                (org-agenda-list-stuck-projects) 
  ;;                (tags-todo "-REFILE+Effort=\"\""
  ;;                           ((org-agenda-overriding-header "Tasks to Estimate")
  ;;                            (org-agenda-skip-function 'jcs:skip-projects)
  ;;                            (org-tags-match-list-sublevels t)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down effort-up category-keep))))
  ;;                (tags-todo "-REFILE/!STARTED"
  ;;                           ((org-agenda-overriding-header "Tasks in Progress")
  ;;                            (org-agenda-skip-function 'jcs:skip-projects)
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-tags-match-list-sublevels t)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(priority-down effort-up category-keep))))
  ;;                (tags-todo "goal_year"
  ;;                           ((org-agenda-overriding-header "Yearly OKRs")
  ;;                            (org-tags-match-list-sublevels t)
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down effort-up category-keep))))
  ;;                (tags-todo "goal_quarter"
  ;;                           ((org-agenda-overriding-header "Quarterly OKRs")
  ;;                            (org-tags-match-list-sublevels t)
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down effort-up category-keep))))
  ;;                (tags-todo "goal_month"
  ;;                           ((org-agenda-overriding-header "This Month's OKRs")
  ;;                            (org-tags-match-list-sublevels t)
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down effort-up category-keep))))
  ;;                (tags-todo "goal_week"
  ;;                           ((org-agenda-overriding-header "This Week's Goals")
  ;;                            (org-tags-match-list-sublevels t)
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down effort-up category-keep))))
  ;;                (tags-todo "+PRIORITY=\"A\""
  ;;                           ((org-agenda-overriding-header "Tasks for Today")
  ;;                            (org-agenda-skip-function 'jcs:skip-projects)
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-tags-match-list-sublevels t)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(priority-down effort-up category-keep))))
  ;;                ))
  ;;              ("d" "@desk"
  ;;               ((tags-todo "@desk-REFILE" 
  ;;                           ((org-agenda-overriding-header "All Desk/Writing Tasks")
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down effort-up category-keep))))
  ;;               ))
  ;;              ("h" "@home + agenda"
  ;;               ((agenda "")
  ;;                (tags-todo "@home-REFILE/!STARTED" 
  ;;                           ((org-agenda-overriding-header (jcs:wip-text "@home" "STARTED" 1))
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down priority-down effort-up category-keep))))
  ;;                (tags-todo "@home-REFILE/!NEXT"
  ;;                           ((org-agenda-overriding-header (jcs:wip-text "@home" "NEXT" 3))
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down effort-up category-keep))))
  ;;                (tags-todo "@home-REFILE/!WAITING"
  ;;                           ((org-agenda-overriding-header (jcs:wip-text "@home" "WAITING" 3))
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down effort-up category-keep))))
  ;;                (tags-todo "@home-REFILE/"
  ;;                           ((org-agenda-overriding-header (jcs:wip-text "@home" "TODO" 20))
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down effort-up category-keep))))
  ;;                ))
  ;;              ("r" "@errands" tags-todo "@errands")
  ;;              ("g" "goals"
  ;;               ((tags "goal_week"
  ;;                      ((org-agenda-overriding-header "This week's goals")))
  ;;                (tags "goal_month"
  ;;                      ((org-agenda-overriding-header "This month's goals")))
  ;;                (tags "goal_year"
  ;;                      ((org-agenda-overriding-header "This year's goals")))))
  ;;              ("w" "@work + agenda"
  ;;               ((agenda "")
  ;;                (tags-todo "@work-REFILE/!STARTED" 
  ;;                           ((org-agenda-overriding-header (jcs:wip-text "@work" "STARTED" 1))
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down effort-up category-keep))))
  ;;                (tags-todo "@work-REFILE/!NEXT"
  ;;                           ((org-agenda-overriding-header (jcs:wip-text "@work" "NEXT" 5))
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down effort-up category-keep))))
  ;;                (tags-todo "@work-REFILE/!WAITING"
  ;;                           ((org-agenda-overriding-header (jcs:wip-text "@work" "WAITING" 5))
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down effort-up category-keep))))
  ;;                (tags-todo "@work-REFILE/!TODO"
  ;;                           ((org-agenda-overriding-header (jcs:wip-text "@work" "TODO" 20))
  ;;                            (org-agenda-todo-ignore-scheduled 'future)
  ;;                            (org-agenda-sorting-strategy
  ;;                             '(todo-state-down priority-down effort-up category-keep))))
  ;;                ))
  ;;              ("D" "Daily review"
  ;;               agenda ""
  ;;               ((org-agenda-span 'day)
  ;;                (org-agenda-start-on-weekday 0)
  ;;                (org-agenda-start-with-log-mode t)
  ;;                (org-agenda-use-time-grid nil)
  ;;                (org-agenda-toggle-diary nil)
  ;;                (org-agenda-skip-function
  ;;                 '(org-agenda-skip-entry-if 'nottodo 'done))
  ;;                ))
  ;;              ))

    ;; A couple of helper functions for org agendas from Bernt Hansen
    ;;   http://doc.norang.ca/org-mode.html
    (defun jcs:is-project-p ()
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

    (defun jcs:skip-projects ()
      "Skip trees that are projects"
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((jcs:is-project-p)
          next-headline)
         (t
          nil))))


    ;;-----------------------------------------------------------------------------
    ;; Diary and appt settings
    ;;-----------------------------------------------------------------------------
    (setq diary-file (concat org-dir "calendar.diary"))
    (add-hook 'diary-display-hook 'fancy-diary-display)
    (setq diary-list-entries-hook
          '(diary-include-other-diary-files diary-sort-entries))
    (add-hook 'diary-mark-diary-entries-hook 'diary-mark-included-diary-files)

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


    ;;-----------------------------------------------------------------------------
    ;; TODOs and Tags
    ;;-----------------------------------------------------------------------------
    (setq org-default-priority 69
          org-lowest-priority 69
          org-priority-start-cycle-with-default t
          org-enforce-todo-checkbox-dependencies nil
          org-enforce-todo-dependencies nil)

    ;; Org-habit options for tracking repeating 'habit' tasks
    (require 'org-habit)
    (setq org-habit-show-habits-only-for-today nil
          org-habit-show-all-today t)

    ;; Options for setting IDs on TODO items when exporting
    (setq org-id-include-domain nil
          org-id-method (quote uuidgen))


    ;;-----------------------------------------------------------------------------
    ;; Time tracking, logging, & effort estimates
    ;;-----------------------------------------------------------------------------

    ;; My values for time estimates and focus levels
    (setq org-global-properties (quote (("Effort_ALL" .
                                         "0:05 0:15 0:30 1:00 2:00 4:00 8:00")
                                        ("Focus_ALL" . "High Medium Low"))))

    ;; Some basic clocking display options
    (setq org-clock-into-drawer t
          org-clock-sound nil
          org-clock-mode-line-total 'current
          org-clock-history-length 10
          org-clock-clocked-in-display 'mode-line)

    ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)

    ;; I want my clock to display in the frame title.
    ;; This is a quick hack to see if productivity apps recognize this.
    ;; TODO: Do I actually want/need this?
    (add-hook 'org-clock-in-hook 'jcs:clock-in-frame)
    (add-hook 'org-clock-out-hook 'jcs:clock-out-frame)

    (defun jcs:clock-in-frame ()
      (setq frame-title-format '("" "[" org-clock-current-task "]")))

    (defun jcs:clock-out-frame ()
      (setq frame-title-format '("" "%b")))


    ;; Set the default task while at work -- This is the "General organization" task in work.org
    (defvar jcs:work-org-task-id "DB00839E-39A9-4023-8494-25EA0BDCF16D")

    (setq jcs:keep-clock-running nil)

    (defun jcs:clock-in-organization-task-as-default ()
      (interactive)
      (org-with-point-at (org-id-find jcs:work-org-task-id 'marker)
        (org-clock-in '(16))))


      ;;-----------------------------------------------------------------------------
      ;; jcs:getcals --- Sync my Google Calendars to emacs diary
      ;;-----------------------------------------------------------------------------
      (require 'icalendar)

      (defun getcal (url)
        "Download ics file and add to diary"
        (let ((tmpfile (url-file-local-copy url)))
          (icalendar-import-file tmpfile "~/org/calendar.diary" t)
          (kill-buffer (car (last (split-string tmpfile "/"))))
          )
        )

      ;; Grab google calendars from secrets.el.gpg
      (defun jcs:getcals ()
        (interactive)
        (if (not (boundp 'google-calendars))
            (jcs:decrypt-secrets))
          (find-file "~/org/calendar.diary")
          (flush-lines "^[& ]")
          (dolist (url google-calendars) (getcal url))
          (kill-buffer "calendar.diary"))


      ;;-----------------------------------------------------------------------------
      ;; jcs:clock functions --- Functions to clock into/out of  a particular item in
      ;; projects.org (OR create a new item and clock into it)
      ;; (NOTE: Doesn't work at the moment -- fix this)
      ;;-----------------------------------------------------------------------------
       (defun jcs:clock-in-to-string (theString &optional theCategory)
        "Clock into a particular item in ~/org/work.org file. Takes optional Category param."
        (interactive)
        (save-excursion
          (let (filepath filename mybuffer)
            (setq filepath "/Users/jstautz/org/work.org"
                  filename (file-name-nondirectory filepath)
                  mybuffer (find-file filepath))
            (goto-char (point-min))
            (widen) 
            ;; if no category defined, try to find string in file and clock in
            (if (eq theCategory nil)
                (if (search-forward theString nil t)
                    (org-clock-in)
                  ;; if not found in buffer, insert new item at end and clock into it
                  (goto-char (point-max))
                  (insert (concat "*** " theString))
                  (goto-char (point-max))
                  (org-clock-in))
              ;; thecategory is non-nil, so this is a new item w/ category
              (goto-char (point-max))
              (insert (concat "*** " theString "\n  :PROPERTIES:\n  :CATEGORY: " theCategory "\n  :END:\n"))
              (goto-char (point-max))
              (org-clock-in)))))

      (defun jcs:clock-out (&optional theString theCategory)
        (org-clock-out))


    (defun jcs:punch-in (arg)
      "Start continuous clocking and set the default task to the
    selected task.  If no task is selected set the Organization task
    as the default task."
      (interactive "p")
      (setq jcs:keep-clock-running t)
      (if (equal major-mode 'org-agenda-mode)
          ;;
          ;; We're in the agenda
          ;;
          (let* ((marker (org-get-at-bol 'org-hd-marker))
                 (tags (org-with-point-at marker (org-get-tags-at))))
            (if (and (eq arg 4) tags)
                (org-agenda-clock-in '(16))
              (jcs:clock-in-organization-task-as-default)))
        ;;
        ;; We are not in the agenda
        ;;
        (save-restriction
          (widen)
          ; Find the tags on the current task
          (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
              (org-clock-in '(16))
            (jcs:clock-in-organization-task-as-default)))))

    (defun jcs:punch-out ()
      (interactive)
      (setq jcs:keep-clock-running nil)
      (when (org-clock-is-active)
        (org-clock-out))
      (org-agenda-remove-restriction-lock))


    (defun jcs:clock-in-default-task ()
      (save-excursion
        (org-with-point-at org-clock-default-task
          (org-clock-in))))


    (defun jcs:clock-out-maybe ()
      (when (and jcs:keep-clock-running
                 (not org-clock-clocking-in)
                 (marker-buffer org-clock-default-task)
                 (not org-clock-resolving-clocks-due-to-idleness))
        (jcs:clock-in-organization-task-as-default)))

    (add-hook 'org-clock-out-hook 'jcs:clock-out-maybe 'append)

  ;; Shift-up and shift-down move a time stamp +/- 5 mins. But don't round when adding time stamps.
    (setq org-time-stamp-rounding-minutes (quote (0 5)))

    ;; Clock out when moving task to a done state
    (setq org-clock-out-when-done t)

    ;; Idle time / resume options
    (setq org-clock-idle-time 5
          org-clock-in-resume t)

    ;;org clocks if I restart emacs w/ running clock
    (setq org-clock-persist t
          org-clock-persist-file "~/.emacs.d/.org-clock-save.el")
    (org-clock-persistence-insinuate)
    ;; Do not prompt to resume an active clock
    (setq org-clock-persist-query-resume nil)

    ;; Enable auto clock resolution for finding open clocks
    (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

    ;; Include current clocking task in clock reports
    (setq org-clock-report-include-clocking-task t)


    ;; When and how to log TODO changes and scheduling changes
    (setq org-log-done (quote time)
          org-log-into-drawer "LOGBOOK"
          org-log-repeat (quote time))

    ;; Change task to STARTED when clocking in -- from Bernt Hansen
    (setq org-clock-in-switch-to-state 'jcs:clock-in-to-started)

    (defun jcs:clock-in-to-started (kw)
      "Switch task from TODO or NEXT to STARTED when clocking in.
    Skips capture tasks."
      (if (and (member (org-get-todo-state) (list "TODO" "NEXT"))
               (not (and (boundp 'org-capture-mode) org-capture-mode)))
          "STARTED"))

    ;; Get rid of empty clock/property drawers -- from Bernt Hansen
    (defun jcs:remove-empty-drawer-on-clock-out ()
      (interactive)
      (save-excursion
        (beginning-of-line 0)
        (org-remove-empty-drawer-at (point))))

    (add-hook 'org-clock-out-hook 'jcs:remove-empty-drawer-on-clock-out 'append)




    ;;-----------------------------------------------------------------------------
    ;; Capture, Refile, Archive
    ;;-----------------------------------------------------------------------------

    ;; Where to look for refile targets
    ;; TODO figure out a more concise way to to this using org-agenda-files, minus inbox, plus someday
    ;; Note that because of the way my work.org file is organized, I want top-level targets there
    ;; but 2nd-level targets everywhere else.
    (setq org-refile-targets (quote (("/Users/jstautz/org/todo.org" :maxlevel . 2)
                                     ("/Users/jstautz/org/someday_maybe.org" :maxlevel . 2))))


    ;; Archiving options
    (setq org-archive-location (concat org-dir "archives.org::")
          org-archive-mark-done nil)

    ;; Refile to date tree -- useful for refiling into a journal file organized in org datetree format
    ;; NOTE: this is finicky right now and I'm not sure why. Need to review at some point.
    (defun org-refile-to-datetree ()
      "Refile a subtree to a datetree corresponding to its timestamp."
      (interactive)
      (let* ((datetree-date (org-entry-get nil "TIMESTAMP" t))
             (date (org-date-to-gregorian datetree-date)))
        (when date
          (save-excursion
            (org-cut-subtree)
            (org-datetree-find-date-create date nil)
            (org-narrow-to-subtree)
            (show-subtree)
            (org-end-of-subtree t)
            (newline)
            (goto-char (point-max))
            (org-paste-subtree 4)
            (widen)))))

    ;; (defun org-refile-to-datetree (&optional file)
    ;;   "Refile a subtree to a datetree corresponding to it's timestamp.

    ;; The current time is used if the entry has no timestamp. If FILE
    ;; is nil, refile in the current file."
    ;;   (interactive "f")
    ;;   (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
    ;;                             (org-read-date t nil "now")))
    ;;          (date (org-date-to-gregorian datetree-date))
    ;;          )
    ;;     (save-excursion
    ;;       (with-current-buffer (current-buffer)
    ;;         (org-cut-subtree)
    ;;         (if file (find-file file))
    ;;         (org-datetree-find-date-create date)
    ;;         (org-narrow-to-subtree)
    ;;         (show-subtree)
    ;;         (org-end-of-subtree t)
    ;;         (newline)
    ;;         (goto-char (point-max))
    ;;         (org-paste-subtree 4)
    ;;         (widen)
    ;;         ))
    ;;     )
    ;;   )




    ;;-----------------------------------------------------------------------------
    ;; Exporting and Publishing
    ;;-----------------------------------------------------------------------------
    (setq org-export-with-TeX-macros nil
          org-table-export-default-format "orgtbl-to-csv")

    ;; Export calendar options
    (setq org-combined-agenda-icalendar-file (concat org-dir "org.ics")
          org-icalendar-combined-name "Org"
          org-icalendar-include-todo t
          org-icalendar-store-UID t)

    ;; Testing some agenda export functions
    (setq org-agenda-exporter-settings
                    '((htmlize-output-type 'css)))

    ;; Set styles for htmlize agenda export
    (setq org-agenda-export-html-style "<style type=\"text/css\">
           p { font-weight: normal; color: gray; }
           .org-agenda-structure {
              font-size: 110%;
              color: #003399;
              font-weight: 600;
           }
           .org-todo {
              color: #cc6666;
              font-weight: bold;
           }
           .org-agenda-done {
              color: #339933;
           }
           .org-done {
              color: #339933;
           }
           .title { text-align: center; }
           .todo, .deadline { color: red; }
           .done { color: green; }
        </style>")

    ;; Some publishing settings stolen from Bernt Hansen

    ;; Inline images in HTML instead of producting links to the image
    (setq org-html-inline-images t)
    ;; Do not use sub or superscripts - I currently don't need this functionality in my documents
    (setq org-export-with-sub-superscripts nil)
    ; Use org.css from the norang website for export document stylesheets
    (setq org-html-head "<link rel=\"stylesheet\" href=\"org.css\" type=\"text/css\" />")
    (setq org-html-head-include-default-style nil)

    ;;-----------------------------------------------------------------------------
    ;; Notifications -- use terminal-notifier to send org & calendar notifications
    ;;-----------------------------------------------------------------------------

    ;; Send org notifications to terminal-notify
    (setq org-show-notification-handler '(lambda (notification) (terminal-notifier-notify "org-mode notification:" notification)))

    ;; Send Appt reminders to terminal-notify
    (progn
      (appt-activate 1)
      (setq appt-display-format 'window
            appt-disp-window-function (function my-appt-disp-window))
      (defun my-appt-disp-window (min-to-app new-time msg)
        (terminal-notifier-notify "Reminder" (format "%s" msg))))

    ;; Also trying jweigley's alert package (dependency of org-pomodoro)
    (setq alert-default-style 'notifier)

    ;;-----------------------------------------------------------------------------
    ;; org-mobile settings -- for export/sync to iOS app
    ;;-----------------------------------------------------------------------------

    (setq org-mobile-files '(org-agenda-files
                             org-agenda-text-search-extra-files)
          org-mobile-inbox-for-pull (concat org-dir "inbox.org")
          org-mobile-directory (concat home-dir "Dropbox/Apps/MobileOrg")
          org-mobile-use-encryption t)

    ;; decrypt using keys in my secrets.el file
    (add-hook 'org-mobile-pre-push-hook 'jcs:decrypt-secrets)
    (add-hook 'org-mobile-pre-pull-hook 'jcs:decrypt-secrets)

    ;; I don't care about possible leakage in autosave files
    (setq org-crypt-disable-auto-save nil)


    ;;-----------------------------------------------------------------------------
    ;; Fontify source blocks in org-mode (babel)
    ;;-----------------------------------------------------------------------------

    (setq org-src-fontify-natively t)

    ;;-----------------------------------------------------------------------------
    ;; Org-pomodoro setup
    ;;-----------------------------------------------------------------------------

    (use-package org-pomodoro)

    (setq org-pomodoro-manual-break t
          org-pomodoro-keep-killed-pomodoro-time t
          org-pomodoro-finished-sound "/Users/jstautz/.emacs.d/resources/alarm_clock.aif"
          org-pomodoro-overtime-sound org-pomodoro-finished-sound
          org-pomodoro-short-break-sound org-pomodoro-finished-sound
          org-pomodoro-long-break-sound org-pomodoro-finished-sound
          org-pomodoro-ticking-sound-p t
          org-pomodoro-ticking-sound "/Users/jstautz/.emacs.d/resources/ticks.aif"
          org-pomodoro-ticking-sound-states '(:pomodoro)
          org-pomodoro-ticking-frequency 120
          org-pomodoro-short-break-length 6
          org-pomodoro-length 24)

    (defun jcs:org-pomodoro-variable-start (&optional arg)
      "Start a pomodoro with a variable timer. Pomodoro will end after specified time, defaults to 25 minutes."
      (interactive "MPomodoro duration (default 24): ")
      (setq org-pomodoro-length 24)
      (if (>= (string-to-number arg) 1)
          (setq org-pomodoro-length (string-to-number arg)))
      (org-pomodoro))

    (defun jcs:raise-emacs ()
      "Raise Emacs"
      (ns-do-applescript "tell application \"Emacs\" to activate"))

    (defun jcs:org-pomodoro-maybe-extend ()
      "Query the user to extend the pomodoro with additional time, delaying the break."
      (interactive)
      (if (y-or-n-p "Extend current Pomodoro? ")
          (progn
            (setq org-pomodoro-length (string-to-number (read-string "Extend Pomodoro duration: ")))
            (org-pomodoro-start)))
      (org-pomodoro))

    (defun jcs:org-pomodoro-breadcrumbs ()
      (progn
        (switch-to-buffer (get-buffer-create "*Pomodoro Notes*"))
        (org-mode)
        (goto-char (point-min))
        (insert "* ")
        (org-insert-time-stamp (current-time) t t)
        (insert "\n\n\n----------Breadcrumbs----------\n\n<One sentence describing what I was just doing.>\n\nA list of pointers to things I need to remember when I get back:\n\n- <First thing I shouldn’t forget\n- Second thing I shouldn’t forget>\n\nYou do not have to make full sentences, just jotting down individual words that make sense to you is enough. They merely function as pointers back to your memory.\n\n----------Priming for Next Session----------\n\n<One sentence describing what you think you will be doing when you come back from your break.>\n\nNow visualize what doing this will entail:\n\n- Are there any decisions that you need to make before you can continue?\n- Are there any questions that need answering before you can continue?\n- Do you need any specific resources before you can continue?\n\n<One question that needs answering shortly after you come back from your break.>\n\n")))

    (add-hook 'org-pomodoro-finished-hook 'jcs:org-pomodoro-breadcrumbs)

    (add-hook 'org-pomodoro-overtime-hook 'jcs:org-pomodoro-maybe-extend)


    (define-key global-map "\C-cp" 'jcs:org-pomodoro-variable-start)
    (add-to-list 'org-speed-commands (cons "p" 'jcs:org-pomodoro-variable-start))

    ;;-----------------------------------------------------------------------------
    ;; Ditaa setup
    ;;-----------------------------------------------------------------------------

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ditaa . t)))
    (setq org-ditaa-jar-path (concat home-dir "bin/ditaa.jar"))

    (diminish 'org-indent-mode)


    ;;-----------------------------------------------------------------------------
    ;; Setting up org capture. Now that DayOne isn't working, I'll need this
    ;;-----------------------------------------------------------------------------


    (setq org-default-notes-file (concat writing-dir "01-composting/journal.txt"))
    (global-set-key (kbd "C-c c") #'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file (concat org-dir "inbox.org"))
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree org-default-notes-file)
         "* %T %^{PROMPT} %^g\n\n%i %?\n")))
