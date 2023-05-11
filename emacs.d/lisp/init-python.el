;; Run Jupyter notebooks in emacs
(use-package ein)

(use-package poetry)

(use-package pyenv
  :hook (python-mode . pyenv-mode))

(use-package python-mode)

(provide 'init-python)
;;; init-python.el ends here
