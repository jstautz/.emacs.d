
;; Make browse-kill-ring a pop-up window that closes after use, rather than staying open
(add-to-list 'load-path (concat dotemacs-dir "el-get/popwin/misc"))
(require 'popwin-browse-kill-ring)
(push "*Kill Ring*" popwin:special-display-config)

(popwin-mode 1)
