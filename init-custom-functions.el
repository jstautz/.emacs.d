;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A random smattering of custom functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;-----------------------------------------------------------------------------
;; Decrypt and load secrets.el.gpg file containing passwords, etc.
;;-----------------------------------------------------------------------------
(defun jcs:decrypt-secrets ()
  (interactive)
  (require 'secrets))


;;-----------------------------------------------------------------------------
;; Set up Emacs->Growl notifications
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


;;-----------------------------------------------------------------------------
;; Send Appt reminders to Growl
;;-----------------------------------------------------------------------------
(progn
  (appt-activate 1)
  (setq appt-display-format 'window)
  (setq appt-disp-window-function (function my-appt-disp-window))
  (defun my-appt-disp-window (min-to-app new-time msg)
    (growl "Reminder" (format "%s" msg))))


;;-----------------------------------------------------------------------------
;; jcs:getcals -- Sync Google Calendars to emacs diary
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
;; jcs:clock functions -- Functions to clock into/out of  a particular item in
;; projects.org (OR create a new item and clock into it)
;;-----------------------------------------------------------------------------
 (defun jcs:clock-in-to-string (theString &optional theCategory)
  "Clock into a particular item in ~/org/projects.org file. Takes optional Category param."
  (interactive)
  (save-excursion
	(let (filepath filename mybuffer)
	  (setq filepath "/Users/jstautz/org/projects.org")
	  (setq filename (file-name-nondirectory filepath))
	  (setq mybuffer (find-file filepath))
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

;;-----------------------------------------------------------------------------
;; i-search with initial contents.
;; original source: http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
;;-----------------------------------------------------------------------------
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))


(provide 'init-custom-functions)
