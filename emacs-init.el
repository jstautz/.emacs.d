(org-babel-load-file (expand-file-name "emacs-init.org" dotemacs-dir))

;; Load Org-mode from source when the ORG_HOME environment variable is set
(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (require 'org))))

(let ((jcs:org-home (shell-command-to-string ". ~/.bashrc; echo -n $ORG_HOME")))
  (setenv "ORG_HOME" jcs:org-home))

<<Prerequisites and setup -- load basic support libraries, set some useful variables>>
<<Sync external packages with el-get, load them, and configure them>>
<<Load some editor customizations, UI tweaks, and custom keybindings>>
<<Load a bunch of custom functions>>
<<Post-flight: load Emacs desktop and start server>>

(require 'cl)

(let ((jcs:shell-path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" jcs:shell-path)
  (setq exec-path (split-string jcs:shell-path ":")))

(setq home-dir "/Users/jstautz/"
      dotemacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
      emacs-dir "/Applications/Emacs.app/Contents/"
      emacs-bin (concat emacs-dir "MacOS/Emacs")
      info-dir (concat emacs-dir "Resources/info/"))

(setq custom-file (concat dotemacs-dir "init-customizations.el"))

(add-to-list 'load-path dotemacs-dir)

(defun jcs:decrypt-secrets ()
  (interactive)
  (require 'secrets))

(require 'package)
(dolist (source '( ("gnu" . "http://elpa.gnu.org/packages/")
                   ("elpa" . "http://tromey.com/elpa/")
                   ("marmalade" . "http://marmalade-repo.org/packages/")
                   ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(add-to-list 'load-path (concat dotemacs-dir "elpa"))
(add-to-list 'load-path (concat dotemacs-dir "el-get/el-get"))

;; el-get, install thyself!
(unless (require 'el-get nil t)
 (url-retrieve
  "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
  (lambda (s)
    (let ((el-get-master-branch) (el-get-install-skip-emacswiki-recipes))
      (goto-char (point-max))
      (eval-print-last-sexp)))))

(setq el-get-dir (concat dotemacs-dir "el-get")
      el-get-user-package-directory (concat dotemacs-dir "config"))

(require 'init-packages)

(require 'init-customizations)

(require 'init-custom-functions)

(desktop-read)

(server-start)
