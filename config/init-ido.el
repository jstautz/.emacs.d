(ido-mode t)
(setq ido-enable-flex-matching t
      ido-enable-tramp-completion nil
      ido-everywhere t
      ido-is-tramp-root nil
      ido-max-prospects 10
      ido-record-ftp-work-directories nil
      ido-show-dot-for-dired t
      ido-use-virtual-buffers t)
(recentf-mode t)
(put 'ido-exit-minibuffer 'disabled nil)
