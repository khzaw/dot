(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-indent-info nil
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'truncate-upto-project))

(use-package hide-mode-line)

(provide 'init-modeline)
