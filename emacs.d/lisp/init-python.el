;; Run Jupyter notebooks in emacs
(use-package ein)

;; (use-package poetry
;;   :if (executable-find "poetry")
;;   :hook (python-mode . poetry-tracking-mode)
;;   :config
;;   (setq poetry-tracking-strategy 'switch-buffer))

;; (use-package pyenv
;;   :when (executable-find "pyenv")
;;   :disabled t
;;   :straight (:type git :host github :repo "aiguofer/pyenv.el")
;;   :config
;;   (setq pyenv-use-alias 't)
;;   (setq pyenv-modestring-prefix " ")
;;   (setq pyenv-modestring-posfix nil)
;;   (setq pyenv-set-path nil)
;;   (setq pyenv-show-active-python-in-modeline nil)
;;   (global-pyenv-mode)
;;   (defun pyenv-update-on-buffer-switch (prev curr)
;;     (if (string-equal "Python" (format-mode-line mode-name nil nil curr))
;;         (pyenv-use-corresponding)))
;;   (add-hook 'switch-buffer-functions 'pyenv-update-on-buffer-switch))

;; (defun run-command-in-directory (dir cmd &rest args)
;;   "Run a command in the specified directory. If the directory is nil, the directory of the file is used. The stdout result is trimmed of whitespace and returned."
;;   (let (
;;         (default-directory (or dir default-directory))
;;         (stdout-buffer (generate-new-buffer "tmp-stdout" t))
;;         (full-cmd (append '(call-process cmd nil (list stdout-buffer nil) nil) args))
;;         )
;;     (unwind-protect
;;         (let ((exit-status (condition-case nil (eval full-cmd) (file-missing nil))))
;;           (if (eq exit-status 0)
;;               (progn
;;                 (with-current-buffer stdout-buffer
;;                   (string-trim (buffer-string))
;;                   )
;;                 )
;;             )
;;           )
;;       (kill-buffer stdout-buffer)
;;       )
;;     )
;;   )

;; (defun locate-venv-poetry ()
;;   "Find a poetry venv."
;;   (run-command-in-directory nil "poetry" "env" "info" "-p"))

(use-package python-mode
  :disabled t
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


(use-package pet
  :straight (:host github :repo "wyuenho/emacs-pet")
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))
              (pet-eglot-setup)
              (eglot-ensure)
              (setq-local lsp-jedi-executable-command
                          (pet-executable-find "jedi-language-server"))
              (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter
                          lsp-pyright-venv-path python-shell-virtualenv-root)

              ;; (lsp)

              (setq-local dap-python-executable python-shell-interpreter)

              (setq-local python-pytest-executable (pet-executable-find "pytest"))

              (when-let ((ruff-executable (pet-executable-find "ruff")))
                (setq-local ruff-format-command ruff-executable)
                (ruff-format-on-save-mode))

              (when-let ((black-executable (pet-executable-find "black")))
                (setq-local python-black-command black-executable)
                (python-black-on-save-mode))

              (when-let ((isort-executable (pet-executable-find "isort")))
                (setq-local python-isort-command isort-executable)
                (python-isort-on-save-mode))
              )))

(provide 'init-python)
;; init-python.el ends here
