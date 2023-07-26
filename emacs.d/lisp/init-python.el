;; Run Jupyter notebooks in emacs
(use-package ein)

(use-package poetry
  :hook (python-mode . poetry-tracking-mode)
  :config
  (setq poetry-tracking-strategy 'switch-buffer))

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

(defun run-command-in-directory (dir cmd &rest args)
  "Run a command in the specified directory. If the directory is nil, the directory of the file is used. The stdout result is trimmed of whitespace and returned."
  (let (
        (default-directory (or dir default-directory))
        (stdout-buffer (generate-new-buffer "tmp-stdout" t))
        (full-cmd (append '(call-process cmd nil (list stdout-buffer nil) nil) args))
        )
    (unwind-protect
        (let ((exit-status (condition-case nil (eval full-cmd) (file-missing nil))))
          (if (eq exit-status 0)
              (progn
                (with-current-buffer stdout-buffer
                  (string-trim (buffer-string))
                  )
                )
            )
          )
      (kill-buffer stdout-buffer)
      )
    )
  )

(defun locate-venv-poetry ()
  "Find a poetry venv."
  (run-command-in-directory nil "poetry" "env" "info" "-p"))



(use-package auto-virtualenv
  :init
  (use-package pyvenv)
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv))

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :commands python-mode
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"))
  :hook
  ((python-mode . (lambda ()
                    (when (executable-find "poetry")
                      (let ((venv (locate-venv-poetry))) (when venv
                                                           (setq-default eglot-workspace-configuration
                                                                         (list (cons ':python (list ':venvPath venv ':pythonPath (concat venv "/bin/python"))))))))
                    (eglot-ensure)))))

(provide 'init-python)
;;; init-python.el ends here
