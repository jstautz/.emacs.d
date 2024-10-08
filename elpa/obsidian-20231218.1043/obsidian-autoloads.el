;;; obsidian-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "obsidian" "obsidian.el" (0 0 0 0))
;;; Generated autoloads from obsidian.el

(defvar obsidian-wiki-link-create-file-in-inbox t "\
Controls where to create a new file from a wiki link if its target is missing.
    If it is true, create in inbox, otherwise next to the current buffer.")

(custom-autoload 'obsidian-wiki-link-create-file-in-inbox "obsidian" t)

(autoload 'obsidian-specify-path "obsidian" "\
Specifies obsidian folder PATH to obsidian-folder variable.

When run interactively asks user to specify the path.

\(fn &optional PATH)" t nil)

(autoload 'obsidian-insert-wikilink "obsidian" "\
Insert a link to file in wikilink format.

If ARG is set, the value of `obsidian-links-use-vault-path' will be toggled for
the current link insertion.

\(fn &optional ARG)" t nil)

(autoload 'obsidian-insert-link "obsidian" "\
Insert a link to file in markdown format.

If ARG is set, the value of `obsidian-links-use-vault-path' will be toggled for
this link insertion. If text is highlighted, the highlighted text will be
replaced by the link.

\(fn &optional ARG)" t nil)

(autoload 'obsidian-capture "obsidian" "\
Create new obsidian note.

In the `obsidian-inbox-directory' if set otherwise in `obsidian-directory' root." t nil)

(autoload 'obsidian-daily-note "obsidian" "\
Create new obsidian daily note.

In the `obsidian-daily-notes-directory' if set otherwise in `obsidian-inbox-directory' - if that's also unset,
in `obsidian-directory' root.
." t nil)

(autoload 'obsidian-jump "obsidian" "\
Jump to Obsidian note." t nil)

(autoload 'obsidian-move-file "obsidian" "\
Move current note to another directory." t nil)

(autoload 'obsidian-follow-link-at-point "obsidian" "\
Follow thing at point if possible, such as a reference link or wiki link.
Opens inline and reference links in a browser.  Opens wiki links
to other files in the current window, or another window if
ARG is non-nil.
See `markdown-follow-link-at-point' and
`markdown-follow-wiki-link-at-point'.

\(fn &optional ARG)" t nil)

(autoload 'obsidian-backlink-jump "obsidian" "\
Select a backlink to this file and follow it." t nil)

(autoload 'obsidian-search "obsidian" "\
Search Obsidian vault for input." t nil)

(autoload 'obsidian-tag-find "obsidian" "\
Find all notes with a tag." t nil)

(put 'global-obsidian-mode 'globalized-minor-mode t)

(defvar global-obsidian-mode nil "\
Non-nil if Global Obsidian mode is enabled.
See the `global-obsidian-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-obsidian-mode'.")

(custom-autoload 'global-obsidian-mode "obsidian" nil)

(autoload 'global-obsidian-mode "obsidian" "\
Toggle Obsidian mode in all buffers.
With prefix ARG, enable Global Obsidian mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Obsidian mode is enabled in all buffers where `obsidian-enable-minor-mode' would do it.

See `obsidian-mode' for more information on Obsidian mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "obsidian" '("obsidian-"))

;;;***

;;;### (autoloads nil nil ("obsidian-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; obsidian-autoloads.el ends here
