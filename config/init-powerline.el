(require 'powerline)

;;-----------------------------------------------------------------------------
;; My updated powerline theme (based on default theme)
;;-----------------------------------------------------------------------------

(defun jcs:powerline-default-theme ()
  "Setup a default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (face1 (if active 'powerline-active1
                                   'powerline-inactive1))
                          (face2 (if active 'powerline-active2
                                   'powerline-inactive2))
                          (lhs (list
                                (powerline-raw "%*" nil 'l)
                                (powerline-buffer-size nil 'l)
                                (powerline-buffer-id nil 'l)

                                (powerline-raw " ")
                                (powerline-arrow-right nil face1)

                                (when (boundp 'erc-modified-channels-object)
                                  (powerline-raw erc-modified-channels-object
                                                 face1 'l))

                                (powerline-major-mode face1 'l)
                                (powerline-process face1)
                                (powerline-minor-modes face1 'l)
                                (powerline-narrow face1 'l)

                                (powerline-raw " " face1)
                                (powerline-arrow-right face1 face2)

                                (powerline-vc face2)))
                          (rhs (list
                                (powerline-raw global-mode-string face2 'r)

                                (powerline-arrow-left face2 face1)

                                (powerline-raw "%4l" face1 'r)
                                (powerline-raw ":" face1)
                                (powerline-raw "%3c" face1 'r)

                                (powerline-arrow-left face1 nil)
                                (powerline-raw " ")

                                (powerline-raw "%6p" nil 'r)

                                (powerline-raw " " face2)
                                (powerline-hud nil face1))))
                     (concat
                      (powerline-render lhs)
                      (powerline-fill face2 (powerline-width rhs))
                      (powerline-render rhs)))))))



(jcs:powerline-default-theme)
