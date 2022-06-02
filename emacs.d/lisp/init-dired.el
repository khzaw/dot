(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  :config
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq dired-use-ls-dired nil)
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))


  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
            (")" . dired-git-info-mode)))

  (use-package diredfl
    :init (diredfl-global-mode 1))

  (use-package dired-sidebar))



(provide 'init-dired)
;;; init-dired.el ends here
