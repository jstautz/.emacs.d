;; Preflight
(require 'cl)
(defvar *emacs-load-start* (current-time))

;; Path/install configs
(setq home-dir "/Users/jstautz/")
(setq dotemacs-dir (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'load-path dotemacs-dir)
(setq emacs-dir "/Applications/Emacs.app/Contents/")
(setq emacs-bin (concat emacs-dir "MacOS/Emacs"))
(setq info-dir (concat emacs-dir "Resources/info/"))

;; Load all packages (uses el-get)
(require 'init-packages)

;; Load all my org-mode configs
(require 'init-org)

;; interface/editing customizations
(require 'init-customizations)

;; random smattering of custom + util functions
(require 'init-custom-functions)

;; Let's get this party started.
(server-start)

;; Post-flight
(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                             (- (+ hi lo) (+ (first *emacs-load-start*) (second
                             *emacs-load-start*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom-set-variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-M-RET-may-split-line t)
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3)))
 '(org-agenda-dim-blocked-tasks t)
 '(org-agenda-include-diary t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-sorting-strategy (quote ((agenda time-up priority-down) (todo priority-down) (tags priority-down))))
 '(org-agenda-span (quote week))
 '(org-agenda-start-with-clockreport-mode nil)
 '(org-agenda-start-with-follow-mode nil)
 '(org-agenda-tags-column -120)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-scheduled (quote future))
 '(org-agenda-todo-list-sublevels t)
 '(org-archive-location (concat org-directory "archives.org::"))
 '(org-archive-mark-done nil)
 '(org-attach-auto-tag nil)
 '(org-babel-no-eval-on-ctrl-c-ctrl-c t)
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item))))
 '(org-clock-history-length 5)
 '(org-clock-idle-time 5)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state (quote bh/clock-in-to-started))
 '(org-clock-into-drawer t)
 '(org-clock-modeline-total (quote current))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-persist t)
 '(org-clock-sound nil)
 '(org-combined-agenda-icalendar-file (concat org-directory "org.ics"))
 '(org-completion-use-ido t)
 '(org-confirm-shell-link-function (quote y-or-n-p))
 '(org-default-priority 69)
 '(org-enforce-todo-checkbox-dependencies nil)
 '(org-enforce-todo-dependencies nil)
 '(org-export-with-TeX-macros nil)
 '(org-fast-tag-selection-single-key t)
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . default) ("\\.celtx\\'" . system) ("\\.doc\\'" . system) ("\\.xls\\'" . system) ("\\.fdr\\'" . system) ("\\.dvi\\'" . system))))
 '(org-fontify-done-headline t)
 '(org-global-properties (quote (("Effort_ALL" . "0:05 0:10 0:15 0:25 1:00 2:00 4:00 8:00") ("Focus_ALL" . "High Medium Low"))))
 '(org-habit-show-habits-only-for-today nil)
 '(org-hide-leading-stars t)
 '(org-icalendar-combined-name "Org")
 '(org-icalendar-include-todo t)
 '(org-icalendar-store-UID t)
 '(org-id-include-domain nil)
 '(org-id-method (quote uuidgen))
 '(org-level-color-stars-only t)
 '(org-log-done (quote time))
 '(org-log-into-drawer "LOGBOOK")
 '(org-log-redeadline (quote note))
 '(org-log-reschedule (quote note))
 '(org-lowest-priority 69)
 '(org-modules (quote (org-bbdb org-bibtex org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-habit)))
 '(org-odd-levels-only t)
 '(org-priority-start-cycle-with-default t)
 '(org-refile-targets (quote ((("~/org/projects.org" "~/org/someday_maybe.org") :maxlevel . 2))))
 '(org-return-follows-link t)
 '(org-reverse-note-order (quote (("inbox" . t))))
 '(org-show-following-heading (quote ((default))))
 '(org-show-hierarchy-above (quote ((default . t) (tags-tree))))
 '(org-show-notification-handler (quote (lambda (notification) (growl "org-mode notification" notification))))
 '(org-show-siblings (quote ((default) (isearch t))))
 '(org-special-ctrl-a/e t)
 '(org-table-export-default-format "orgtbl-to-csv")
 '(org-tags-column 80)
 '(org-use-speed-commands t))

