;;; early-init.el --- Pre-GUI startup -*- lexical-binding: t; -*-

;;; Native compilation (macOS 27+)
;; libgccjit computes -mmacosx-version-min=18.0 on macOS 27, which clang rejects.
(defvar native-comp-driver-options)
(when (and (eq system-type 'darwin) (native-comp-available-p))
  (let ((major (string-to-number
                (car (process-lines "sw_vers" "-productVersion")))))
    (when (>= major 27)
      (setq native-comp-driver-options
            (list (format "-mmacosx-version-min=%d.0" major)))
      (setenv "MACOSX_DEPLOYMENT_TARGET" (format "%d.0" major)))))

;;; early-init.el ends here
