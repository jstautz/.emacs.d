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
                         "~/org/projects.org"))
;; TODO: why can't I replace these with (concat org-dir "filename.org")?
;;  gives me "Wrong type argument: stringp, (concat org-dir "flagged.org")"

;; Search on other files, too
(setq org-agenda-text-search-extra-files '("~/org/goals.org"
                                           "~/org/someday_maybe.org"
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
               (tags "@work-REFILE-noexport+TODO=\"DONE\"+CATEGORY=\"Work\"+CLOSED>=\"[2013-08-01]\"+CLOSED<=\"[2013-08-02]\""
                     ((org-agenda-overriding-header "Completed Today")))
               (tags-todo "@work-REFILE/!STARTED" 
						  ((org-agenda-overriding-header "Doing")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
               (tags-todo "@work-REFILE/!NEXT"
                          ((org-agenda-overriding-header "To Do Today")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
               (tags-todo "@work-REFILE/!WAITING"
                          ((org-agenda-overriding-header "Impeded / Waiting Response")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
               (tags-todo "@work-REFILE/!TODO"
                          ((org-agenda-overriding-header "Queue")
						   (org-agenda-todo-ignore-scheduled 'future)
						   (org-agenda-sorting-strategy
							'(todo-state-down effort-up category-keep))))
               ))
             ("p" "public @work todos"
              ((tags "@work-REFILE-noexport+TODO=\"DONE\"+CATEGORY=\"Work\"+CLOSED>=\"[2013-08-01]\"+CLOSED<=\"[2013-08-02]\""
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
               (tags "@work-REFILE-noexport+TODO=\"DONE\"+CATEGORY=\"Work\"+CLOSED>=\"[2013-07-31]\"+CLOSED<=\"[2013-08-01]\""
                     ((org-agenda-overriding-header "Completed Yesterday")))
               (tags "@work-REFILE-noexport+TODO=\"DONE\"+CATEGORY=\"Work\"+CLOSED>=\"[2013-07-30]\"+CLOSED<=\"[2013-07-31]\""
                     ((org-agenda-overriding-header "Completed [2013-07-30]")))
               (tags "@work-REFILE-noexport+TODO=\"DONE\"+CATEGORY=\"Work\"+CLOSED>=\"[2013-07-29]\"+CLOSED<=\"[2013-07-30]\""
                     ((org-agenda-overriding-header "Completed [2013-07-29]")))
               (tags "@work-REFILE-noexport+TODO=\"DONE\"+CATEGORY=\"Work\"+CLOSED>=\"[2013-07-26]\"+CLOSED<=\"[2013-07-27]\""
                     ((org-agenda-overriding-header "Completed [2013-07-26]")))
               (tags "@work-REFILE-noexport+TODO=\"DONE\"+CATEGORY=\"Work\"+CLOSED>=\"[2013-07-25]\"+CLOSED<=\"[2013-07-26]\""
                     ((org-agenda-overriding-header "Completed [2013-07-25]")))
               (tags "@work-REFILE-noexport+TODO=\"DONE\"+CATEGORY=\"Work\"+CLOSED>=\"[2013-07-24]\"+CLOSED<=\"[2013-07-25]\""
                     ((org-agenda-overriding-header "Completed [2013-06-24]")))
               (tags "@work-REFILE-noexport+TODO=\"DONE\"+CATEGORY=\"Work\"+CLOSED>=\"[2013-07-23]\"+CLOSED<=\"[2013-07-24]\""
                     ((org-agenda-overriding-header "Completed [2013-06-23]")))
               (tags "@work-REFILE-noexport+TODO=\"DONE\"+CATEGORY=\"Work\"+CLOSED>=\"[2013-07-22]\"+CLOSED<=\"[2013-07-23]\""
                     ((org-agenda-overriding-header "Completed [2013-06-22]")))
               
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
      org-clock-modeline-total (quote current)
      org-clock-history-length 5)

;; My values for time estimates and focus levels
(setq org-global-properties (quote (("Effort_ALL" .
                                     "0:05 0:15 0:30 1:00 2:00 4:00 8:00")
                                    ("Focus_ALL" . "High Medium Low"))))

;; Idle time / resume options
(setq org-clock-idle-time 5
      org-clock-in-resume t)

;;org clocks if I restart emacs w/ running clock
(setq org-clock-persist t)
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
    (org-remove-empty-drawer-at "CLOCK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)
(setq org-clock-out-remove-zero-time-clocks t)


;;-----------------------------------------------------------------------------
;; Capture, Refile, Archive
;;-----------------------------------------------------------------------------

;; Use Remember for note capture
;;   (technically deprecated, but I like it more than org-capture)
(org-remember-insinuate)
(setq org-default-notes-file (concat org-dir "inbox.txt"))
(define-key global-map "\C-cr" 'org-remember)
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; Remember templates for org-remember:
;;   t = new TODO item
;;   j = new journal entry (dated today)
;;   i = new inbox item
;;   q = only used via QuickSilver -- new inbox item
     (setq org-remember-templates
      '(("Todo" ?t "*** TODO %^{Action to be taken} %^G\n    %?\n    Added: %U"
         (concat org-dir "projects.org") "Unfiled")
        ("Journal" ?j "* %U %^{Title} %^g\n\n%?\n"
         (concat writing-dir "01-composting/journal.txt") top)
        ("Inbox" ?i "* %U %^{Title}\n  %i%?\n   %a\n\n"
         (concat org-dir "inbox.txt") top)
        ("oldQS remember" ?f "* %U %?\n%i\n%a\n\n"
         (concat org-dir "inbox.txt") top)
        ("QS remember" ?q "* %i\n\n  Added: %U"
         (concat org-dir "inbox.txt") top)
        ("AppleScript remember" ?y "* %:shortdesc\n  %:initial\n   Source: %u, %c\n\n  %?"
         (concat org-dir "inbox.txt"))
        ("AppleScript note" ?z "* %?\n\n  Date: %u\n"
         (concat org-dir "inbox.txt"))))

;; When taking notes in inbox, put newest at top
(setq org-reverse-note-order (quote (("inbox" . t))))

;; Use org-annotation-quicksilver to send items from QS (or Alfred, etc)
;;   (also deprecated, but haven't got its successor, org-mac-protocol,
;;    to work the way I want it yet.)
;;(require 'org-annotation-quicksilver)

;; Where to look for refile targets
(setq org-refile-targets (quote ((("~/org/projects.org"
                                   "~/org/someday_maybe.org") :maxlevel . 2))))

;; Archiving options
(setq org-archive-location (concat org-dir "archives.org::")
      org-archive-mark-done nil)


;;-----------------------------------------------------------------------------
;; Custom link types
;;-----------------------------------------------------------------------------
(org-add-link-type "jira" 'org-jira-open)

(setq org-jira-url "http://jira.hootsuitemedia.com/")

(defun org-jira-open (issue)
  "Visit details page for JIRA issue.
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

(setq org-mobile-files '("~/org/projects.org"
                         "~/org/inbox.txt"
                         "~/org/notes/book_list.org"
                         "~/org/goals.org"
                         "~/org/someday_maybe.org"
                         "~/org/notes/gift_ideas.org")
      org-mobile-inbox-for-pull (concat org-dir "inbox.txt")
      org-mobile-directory (concat home-dir "Dropbox/Apps/MobileOrg")
      org-mobile-use-encryption t)

;; decrypt using keys in my secrets.el file
(add-hook 'org-mobile-pre-push-hook 'jcs:decrypt-secrets)
(add-hook 'org-mobile-pre-pull-hook 'jcs:decrypt-secrets)

;; I don't care about possible leakage in autosave files
(setq org-crypt-disable-auto-save nil)


(provide 'init-org)
