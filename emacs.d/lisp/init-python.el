;; -*- lexical-binding: t; -*-

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

(use-package python-pytest)

(use-package python-black
  :if (executable-find "black"))

(use-package python-isort
  :if (executable-find "isort"))

(use-package ruff-format
  :if (executable-find "ruff"))

(use-package python-mode
  :straight (python-mode :type git
                         :host gitlab
                         :repo "python-mode-devs/python-mode")
  :config
  (setq py-indent-offset 4)
  (setq python-indent-offset 4)
  ;; Remove guess indent python message
  ;; (setq python-indent-guess-indent-offset-verbose nil)
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i"))

   ;; Only allow the python-mode's capf to run in python buffers:
  (defun khz/python-capf-only-in-python-modes (fn &rest args)
    (when (and (derived-mode-p 'python-mode 'inferior-python-mode)
               (not (bound-and-true-p leetcode-solution-mode))
               (not (bound-and-true-p org-src-mode)))
      (apply fn args)))
  (advice-add 'python-shell-completion-at-point :around #'khz/python-capf-only-in-python-modes)

  ;; If you want, you can do the same for py-fast-complete or similar
  (when (fboundp 'py-fast-complete)
    (advice-add 'py-fast-complete :around #'khz/python-capf-only-in-python-modes))

  (advice-add 'python-shell-completion-at-point :around
            (lambda (fun &optional arg)
              (cape-wrap-noninterruptible (lambda () (funcall fun arg)))))

  (defun turn-off-corfu-auto ()
    "turn off corfu-auto"
    (setq-local corfu-auto nil)
    (setq-local corfu-auto-delay 3.0)
    (corfu-mode -1)
    (corfu-mode 1))

  (add-hook 'inferior-python-mode-hook 'turn-off-corfu-auto)
  (add-hook 'py-shell-mode-hook 'turn-off-corfu-auto))
  ;; (remove-hook 'completion-at-point-functions #'python-completion-at-point t))

(use-package python-mls
  :disabled t
  ;; :custom
  ;; (python-mls-multiline-history-modifier '(meta shift))
  :straight (python-mls :type git
                        :host github
                        :repo "jdtsmith/python-mls")
  :hook
  (inferior-python-mode . python-mls-mode))


(use-package pet
  :straight (:host github :repo "wyuenho/emacs-pet")
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "ipython")
                          python-shell-virtualenv-root (pet-virtualenv-root))
              (setq-local python-indent-offset 4)
              (setq-local py-indent-offset 4)
              (pet-eglot-setup)
              (eglot-ensure)
              ;; (pet-flycheck-setup)
              ;; (flycheck-mode)
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
              ))
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package tomlparse
  :straight (:type git :host github :repo "johannes-mueller/tomlparse.el"))

(use-package uv
  :straight (uv :type git :host github :repo "johannes-mueller/uv.el"))

(provide 'init-python)
;; init-python.el ends here
