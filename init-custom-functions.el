;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A random smattering of custom/helper functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;-----------------------------------------------------------------------------
;; Decrypt and load secrets.el.gpg file containing passwords, etc.
;;-----------------------------------------------------------------------------
(defun jcs:decrypt-secrets ()
  (interactive)
  (require 'secrets))


;;-----------------------------------------------------------------------------
;; jcs:getcals -- Sync my Google Calendars to emacs diary
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
	  (setq filepath "/Users/jstautz/org/projects.org"
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


;;-----------------------------------------------------------------------------
;; i-search with initial contents.
;; original src: http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
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


;;-----------------------------------------------------------------------------
;; Nice functions to change copy-kill region to copy or kill current line
;; if no active region.
;; Stolen from http://xahlee.org/emacs/emacs_copy_cut_current_line.html
;;-----------------------------------------------------------------------------
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is copied.")
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))


;;-----------------------------------------------------------------------------
;; Fix yanking into Emacs terminal
;; Mad props to Brian Zwahr
;; http://emacs-journey.blogspot.ca/2012/06/improving-ansi-term.html?m=1
;;-----------------------------------------------------------------------------

(defun my-term-paste (&optional string)
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (if string string (current-kill 0))))


;;-----------------------------------------------------------------------------
;; Nice window-splitting and moving functions
;; Thanks to Ignacio Paz Posse
;; http://ignaciopp.wordpress.com/2009/05/23/emacs-manage-windows-split/
;;-----------------------------------------------------------------------------

;; Switch (zoom) between split window config and a single window
(defun toggle-windows-split()
"Switch back and forth between one window and whatever split of
windows we might have in the frame. The idea is to maximize the
current buffer, while being able to go back to the previous split
of windows in the frame simply by calling this command again."
(interactive)
(if (not(window-minibuffer-p (selected-window)))
(progn
(if (< 1 (count-windows))
(progn
(window-configuration-to-register ?u)
(delete-other-windows))
(jump-to-register ?u))))
(my-iswitchb-close))
(define-key global-map (kbd "C-`") 'toggle-windows-split)
(define-key global-map (kbd "C-~") 'toggle-windows-split)

;; Window shifting. C-x-o lets us go forward a window (or several).
;; This one lets us go back one or more windows. From Glickstein.
(defun other-window-backward (&optional n)
"Select previous Nth window."
(interactive "P")
(other-window (- (prefix-numeric-value n))))
(global-set-key [prior] 'other-window)
(global-set-key [next] 'other-window-backward)
(global-set-key [(control tab)] 'other-window)
(global-set-key [(shift control tab)] 'other-window-backward)

;; Also (stolen from someone else’s dot file) this is used to shrink/expand windows without using the mouse.
(define-key global-map (kbd "C-M-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "C-M-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "C-M-<up>") 'enlarge-window)
(define-key global-map (kbd "C-M-<down>") 'shrink-window)


(provide 'init-custom-functions)
