;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A random smattering of custom/helper functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;; Eval and replace -- replace current sexp with its value
;;-----------------------------------------------------------------------------

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))

(global-set-key (kbd "C-c e") 'eval-and-replace)


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
;; Make zap-to-char act more like "zap-up-to-char"
;; Thanks to Eric Himmelreich
;; http://rawsyntax.com/blog/learn-emacs-use-defadvice-modify-functions/
;;-----------------------------------------------------------------------------
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))


;;-----------------------------------------------------------------------------
;; Window-splitting and moving functions
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
          (jump-to-register ?u)))))

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

;; Also (stolen from someone else's dot file) this is used to shrink/expand windows without using the mouse.
(define-key global-map (kbd "C-M-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "C-M-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "C-M-<up>") 'enlarge-window)
(define-key global-map (kbd "C-M-<down>") 'shrink-window)


;;-----------------------------------------------------------------------------
;; Open in Marked.app -- Stolen from https://github.com/mattsears/emacs
;;-----------------------------------------------------------------------------
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
           (shell-quote-argument (buffer-file-name)))))

(global-set-key "\C-cm" 'markdown-preview-file)


;;-----------------------------------------------------------------------------
;; Re-open buffer as root -- Thank to @christopherdone: http://t.co/KiAWcJoo
;;-----------------------------------------------------------------------------
(defun tramp-sudo-reopen ()
  "Re-open the current with tramp."
  (interactive)
  (let ((file-name (format "/sudo:localhost:%s" (buffer-file-name)))
        (line (line-number-at-pos))
        (column (current-column)))
    (kill-buffer)
    (find-file file-name)
    (goto-line line)
    (goto-char (+ (point) column))))


;;-----------------------------------------------------------------------------
;; auto-recompile elisp -- Thanks to Adolfo Benedetti and Xah Lee
;;-----------------------------------------------------------------------------
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)


;;-----------------------------------------------------------------------------
;; A smarter find-tag that automagically reruns etags when it can't find a
;; requested item and then makes a new try to locate it.
;; by Jonas.Jarnestrom<at>ki.ericsson.se
;; Fri Mar 15 09:52:14 2002
;;-----------------------------------------------------------------------------
(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

;; While we're at it, modify pop-tag-mark to stay centered on cursor
(defadvice pop-tag-mark (after my-pop-tag-mark-advice activate)
  "After popping back to where find-tag was invoked,
   center screen on cursor"
  (let ((current-prefix-arg '(4)))
  (call-interactively 'recenter-top-bottom)))


;;-----------------------------------------------------------------------------
;; Renaming and deleting files should be easier to do. 
;; C-x C-r renames current file
;; C-x C-k deletes current file (since C-x k kills the buffer)
;;-----------------------------------------------------------------------------

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)


(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)


;;-----------------------------------------------------------------------------
;; Make beginning-of-buffer and end-of-buffer work nicely in dired
;; Thanks to @magnars
;;-----------------------------------------------------------------------------

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)


(provide 'init-custom-functions)
