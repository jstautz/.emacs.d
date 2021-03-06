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

;; Extra org modules to load
(setq org-modules (quote (org-bbdb
                          org-gnus
                          org-habit
                          org-info
                          org-jsinfo
                          org-mac-message)))

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

;; Outline structure/style
(setq org-odd-levels-only t
      org-hide-leading-stars t
      org-level-color-stars-only t
      org-fontify-done-headline t
      org-blank-before-new-entry (quote ((heading) (plain-list-item)))
      org-tags-column 80)

;; Editing/Movement tweaks -- turn on speed commands, fast tags, and ido
(setq org-use-speed-commands t
      org-completion-use-ido t
      org-fast-tag-selection-single-key t)

;; Editing/Movement tweaks -- handling line navigation, links, code blocks
(setq org-special-ctrl-a/e t
      org-M-RET-may-split-line t
      org-return-follows-link t
      org-babel-no-eval-on-ctrl-c-ctrl-c t
      org-confirm-shell-link-function (quote y-or-n-p))

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

;; Don't add :ATTACH: tags
(setq org-attach-auto-tag nil)

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


;;-----------------------------------------------------------------------------
;; Agenda setup
;;-----------------------------------------------------------------------------
(setq org-agenda-files '("~/org/inbox.txt"
                         "~/org/personal.org"
                         "~/org/work.org"))

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
      org-agenda-start-with-follow-mode nil)

;; Default agenda views & sorting
(setq org-agenda-include-diary t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-unavailable-files t
      org-agenda-sorting-strategy (quote ((agenda time-up priority-down) (todo priority-down) (tags priority-down)))
      org-agenda-span (quote day))

;; Agenda TODO options
(setq org-agenda-tags-todo-honor-ignore-options t
      org-agenda-todo-ignore-scheduled (quote future)
      org-agenda-todo-list-sublevels t)

;; Options for clock reports in agenda
(setq org-agenda-start-with-clockreport-mode nil
      org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3)))

;; Definition of a "stuck project" for agenda
(setq org-stuck-projects (quote ("+LEVEL=2-REFILE-UNFILED-HABITS/-DONE"
  ("TODO" "NEXT" "STARTED") ("NOTES") "")))

;; Custom function to return agenda header strings based on WIP limit
(defun jcs:wip-text (tags todo limit)
  "Return string to indicate whether WIP limit is exceeded for a
particular tag/todo keyword/limit. For use in
agenda-overriding-header functions. If limit exceeded, string
returned is wrapped in #s"
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
    (concat "# " org-wip-header-text " #")))



;; My custom Agenda commands
(setq org-agenda-custom-commands
           '(
             (" " "Agenda overview"
              ((agenda"")
			   (tags "REFILE" 
					 ((org-agenda-overriding-header "Tasks to Refile")))
			   (org-agenda-list-stuck-projects) 
			   (tags-todo "-REFILE+Effort=\"\""
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
			   (tags-todo "@errands-REFILE"
						  ((org-agenda-overriding-header "Errands")
						   (org-tags-match-list-sublevels t)
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
             ("w" "@work + agenda"
              ((agenda "")
               (tags (concat "@work-REFILE+TODO=\"DONE\"+CLOSED>=\""
                             (format-time-string "[%Y-%m-%d]") "\"+CLOSED<=\""
                             (format-time-string "[%Y-%m-%d]" (time-add (current-time) (days-to-time 1))) "\"")
                     ((org-agenda-overriding-header "Completed Today")))
               (tags-todo "@work-REFILE/!STARTED" 
						  ((org-agenda-overriding-header (jcs:wip-text "@work" "STARTED" 1))
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
               (tags-todo "@work-REFILE/!NEXT"
                          ((org-agenda-overriding-header (jcs:wip-text "@work" "NEXT" 3))
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
               (tags-todo "@work-REFILE/!WAITING"
                          ((org-agenda-overriding-header (jcs:wip-text "@work" "WAITING" 1))
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
               (tags-todo "@work-REFILE/!TODO"
                          ((org-agenda-overriding-header (jcs:wip-text "@work" "TODO" 15))
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
               (tags (concat "@work-REFILE+TODO=\"DONE\"+CLOSED>=\""
                             (format-time-string "[%Y-%m-%d]"
                                                 (time-subtract (current-time) (days-to-time 1)))
                             "\"+CLOSED<=\""
                             (format-time-string "[%Y-%m-%d]") "\"")
                     ((org-agenda-overriding-header "Completed Yesterday")))
               (tags (concat "@work-REFILE+TODO=\"DONE\"+CLOSED>=\""
                             (format-time-string "[%Y-%m-%d]"
                                                 (time-subtract (current-time) (days-to-time 2)))
                             "\"+CLOSED<=\""
                             (format-time-string "[%Y-%m-%d]"
                                                 (time-subtract (current-time) (days-to-time 1))) "\"")
                     ((org-agenda-overriding-header "Completed Two Days Ago")))
               (tags (concat "@work-REFILE+TODO=\"DONE\"+CLOSED>=\""
                             (format-time-string "[%Y-%m-%d]"
                                                 (time-subtract (current-time) (days-to-time 3)))
                             "\"+CLOSED<=\""
                             (format-time-string "[%Y-%m-%d]"
                                                 (time-subtract (current-time) (days-to-time 2))) "\"")
                     ((org-agenda-overriding-header "Completed Three Days Ago")))
               
               ))
             ("p" "public @work todos"
              ((tags (concat "@work-REFILE-noexport+TODO=\"DONE\"+CLOSED>=\""
                             (format-time-string "[%Y-%m-%d]") "\"+CLOSED<=\""
                             (format-time-string "[%Y-%m-%d]" (time-add (current-time) (days-to-time 1))) "\"")
                     ((org-agenda-overriding-header "Completed Today")))
               (tags-todo "@work-REFILE-noexport/!STARTED" 
                          ((org-agenda-overriding-header "Doing")
                           (org-agenda-todo-ignore-scheduled 'future)
                           (org-agenda-sorting-strategy
                            '(todo-state-down effort-up category-keep))))
               (tags-todo "@work-REFILE-noexport/!NEXT"
                          ((org-agenda-overriding-header "To Do Today")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
               (tags-todo "@work-REFILE-noexport/!WAITING"
                          ((org-agenda-overriding-header "Impeded / Waiting Response")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
               (tags-todo "@work-REFILE-noexport/!TODO"
                          ((org-agenda-overriding-header "Queue")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
               (tags (concat "@work-REFILE-noexport+TODO=\"DONE\"+CLOSED>=\""
                             (format-time-string "[%Y-%m-%d]"
                                                 (time-subtract (current-time) (days-to-time 1)))
                             "\"+CLOSED<=\""
                             (format-time-string "[%Y-%m-%d]") "\"")
                     ((org-agenda-overriding-header "Completed Yesterday")))
               (tags (concat "@work-REFILE-noexport+TODO=\"DONE\"+CLOSED>=\""
                             (format-time-string "[%Y-%m-%d]"
                                                 (time-subtract (current-time) (days-to-time 2)))
                             "\"+CLOSED<=\""
                             (format-time-string "[%Y-%m-%d]"
                                                 (time-subtract (current-time) (days-to-time 1))) "\"")
                     ((org-agenda-overriding-header "Completed Two Days Ago")))
               (tags (concat "@work-REFILE-noexport+TODO=\"DONE\"+CLOSED>=\""
                             (format-time-string "[%Y-%m-%d]"
                                                 (time-subtract (current-time) (days-to-time 3)))
                             "\"+CLOSED<=\""
                             (format-time-string "[%Y-%m-%d]"
                                                 (time-subtract (current-time) (days-to-time 2))) "\"")
                     ((org-agenda-overriding-header "Completed Three Days Ago")))

               (tags "@work-REFILE-noexport+TODO=\"DONE\"+CLOSED>=\"[2013-11-28]\"+CLOSED<=\"[2013-11-27]\""
                     ((org-agenda-overriding-header "Completed 2013-11-28")))
			   )
               nil
               ("~/Desktop/work.html"))
			 ))

;; A couple of helper functions for org agendas from Bernt Hansen
;;   http://doc.norang.ca/org-mode.html
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


;;-----------------------------------------------------------------------------
;; Diary and appt settings
;;-----------------------------------------------------------------------------
(setq diary-file (concat org-dir "calendar.diary"))
(add-hook 'diary-display-hook 'fancy-diary-display)
(setq diary-list-entries-hook
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


;;-----------------------------------------------------------------------------
;; TODOs and Tags
;;-----------------------------------------------------------------------------
(setq org-default-priority 69
      org-lowest-priority 69
      org-priority-start-cycle-with-default t
      org-enforce-todo-checkbox-dependencies nil
      org-enforce-todo-dependencies nil)

;; Org-habit options for tracking repeating 'habit' tasks
(setq org-habit-show-habits-only-for-today nil
      org-habit-show-all-today t)

;; Options for setting IDs on TODO items when exporting
(setq org-id-include-domain nil
      org-id-method (quote uuidgen))


;;-----------------------------------------------------------------------------
;; Time tracking, logging, & effort estimates
;;-----------------------------------------------------------------------------
(setq org-clock-into-drawer t
      org-clock-sound nil
      org-clock-mode-line-total 'current
      org-clock-history-length 5
      org-clock-clocked-in-display 'mode-line)

;; My values for time estimates and focus levels
(setq org-global-properties (quote (("Effort_ALL" .
                                     "0:05 0:15 0:30 1:00 2:00 4:00 8:00")
                                    ("Focus_ALL" . "High Medium Low"))))

;; Idle time / resume options
(setq org-clock-idle-time 5
      org-clock-in-resume t)

;;org clocks if I restart emacs w/ running clock
(setq org-clock-persist t
      org-clock-persist-file "~/.emacs.d/.org-clock-save.el")
(org-clock-persistence-insinuate)

;; When and how to log TODO changes and scheduling changes
(setq org-log-done (quote time)
      org-log-into-drawer "LOGBOOK"
      org-log-redeadline (quote note)
      org-log-reschedule (quote note))

;; Change task to STARTED when clocking in -- from Bernt Hansen
(setq org-clock-in-switch-to-state 'bh/clock-in-to-started)

(defun bh/clock-in-to-started (kw)
  "Switch task from TODO or NEXT to STARTED when clocking in.
Skips capture tasks."
  (if (and (member (org-get-todo-state) (list "TODO" "NEXT"))
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      "STARTED"))

;; Get rid of empty clock drawers -- from Bernt Hansen
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)
(setq org-clock-out-remove-zero-time-clocks t)


;;-----------------------------------------------------------------------------
;; Capture, Refile, Archive
;;-----------------------------------------------------------------------------

;; Where to look for refile targets
;; TODO figure out a more concise way to to this using org-agenda-files, minus inbox, plus someday
(setq org-refile-targets (quote ((("/Users/jstautz/org/personal.org"
                                   "/Users/jstautz/org/work.org"
                                   "/Users/jstautz/org/someday_maybe.org") :maxlevel . 2))))

;; Archiving options
(setq org-archive-location (concat org-dir "archives.org::")
      org-archive-mark-done nil)


;;-----------------------------------------------------------------------------
;; Custom link types
;;-----------------------------------------------------------------------------

(jcs:decrypt-secrets)
(org-add-link-type "jira" 'org-jira-open)
(setq org-jira-url org-jira-url)

(defun org-jira-open (issue)
  "Visit details page for JIRA issue on HootSuite's Jira site
     Issue agrument should be a valid issue ID, e.g. AND-123"
  (org-open-link-from-string (concat org-jira-url "browse/" issue)))


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
(setq org-agenda-export-html-style "<style type="text/css">
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

;;-----------------------------------------------------------------------------
;; Notifications -- use Growl to send org & calendar notifications
;;-----------------------------------------------------------------------------
(defvar growl-program "/usr/local/bin/growlnotify")

(defun growl (title message &optional id)
  (if (eq id nil)
      (start-process "growl" " growl"
                     growl-program title "-w" "-a" "iCal.app")
    (start-process "growl" " growl"
                   growl-program title "-w" "-a" "iCal.app" "-d" id))
  (process-send-string " growl" message)
  (process-send-string " growl" "\n")
  (process-send-eof " growl"))

;; Send org notifications to Growl
(setq org-show-notification-handler (quote (lambda (notification) (growl "org-mode notification" notification))))

;; Send Appt reminders to Growl
(progn
  (appt-activate 1)
  (setq appt-display-format 'window
        appt-disp-window-function (function my-appt-disp-window))
  (defun my-appt-disp-window (min-to-app new-time msg)
    (growl "Reminder" (format "%s" msg))))


;;-----------------------------------------------------------------------------
;; org-mobile settings -- for export/sync to iOS app
;;-----------------------------------------------------------------------------

(setq org-mobile-files '(org-agenda-files
                         org-agenda-text-search-extra-files)
      org-mobile-inbox-for-pull (concat org-dir "inbox.txt")
      org-mobile-directory (concat home-dir "Dropbox/Apps/MobileOrg")
      org-mobile-use-encryption t)

;; decrypt using keys in my secrets.el file
(add-hook 'org-mobile-pre-push-hook 'jcs:decrypt-secrets)
(add-hook 'org-mobile-pre-pull-hook 'jcs:decrypt-secrets)

;; I don't care about possible leakage in autosave files
(setq org-crypt-disable-auto-save nil)


(provide 'init-org-mode)
