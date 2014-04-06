(ido-mode t)
(setq ido-enable-flex-matching t
      ido-enable-tramp-completion nil
      ;; since we're using ido-ubiquitous, we don't need this on
      ido-everywhere nil
      ido-is-tramp-root nil
      ido-max-prospects 20
      ;; don't save tramp work directories -- this caused stalls for me in the past
      ido-record-ftp-work-directories nil
      ido-show-dot-for-dired t
      ;; be able to re-visit recently closed buffers
      ido-use-virtual-buffers t
      ;; No automatic searching if no matches found
      ido-auto-merge-work-directories-length -1
      ido-use-faces t
      ;; for find-file-at-point
      ido-use-filename-at-point 'guess)
;; use recentf for visiting recent buffers
(recentf-mode t)
;; don't need this function disabled (it is by default)
(put 'ido-exit-minibuffer 'disabled nil)
