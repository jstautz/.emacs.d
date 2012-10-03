;; From active region to multiple cursors:
(global-set-key (kbd "C-x a l") 'mc/edit-lines)
(global-set-key (kbd "C-x a e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-x a a") 'mc/edit-beginnings-of-lines)

(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C--") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-_") 'mc/mark-more-like-this-extended)

;; Note: expand-region is C-=, so expand-region + mark-next are just a shift key apart

;; (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)
