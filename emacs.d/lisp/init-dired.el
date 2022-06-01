(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-deletes 'always
    dired-recursive-copies 'always)
  (setq dired-use-ls-dired nil)
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))
  (setq dired-listing-switches "-alh --group-directories-first")

  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
            (")" . dired-git-info-mode)))

  (use-package diredfl
    :init (diredfl-global-mode 1)))

(provide 'init-dired)
;;; init-dired.el ends here
