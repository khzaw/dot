;; Run Jupyter notebooks in emacs
(use-package ein)

(use-package poetry)

(use-package pyenv
  :straight (:host github :repo "aiguofer/pyenv.el")
  :config
  (setq pyenv-use-alias 't)
  (setq pyenv-modestring-prefix " ")
  (setq pyenv-modestring-posfix nil)
  (setq pyenv-set-path nil)
  (setq pyenv-show-active-python-in-modeline nil)
  (global-pyenv-mode)
  (defun pyenv-update-on-buffer-switch (prev curr)
    (if (string-equal "Python" (format-mode-line mode-name nil nil curr))
        (pyenv-use-corresponding)))
  (add-hook 'switch-buffer-functions 'pyenv-update-on-buffer-switch))

(use-package python-mode)

(provide 'init-python)
;;; init-python.el ends here
