;; -*- lexical-binding: t; -*-

(use-package shell-maker
  :straight (:type git :host github :repo "xenodium/shell-maker"))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (exec-path-from-shell-copy-env "ANTHROPIC_BASE_URL")
  (exec-path-from-shell-copy-env "ANTHROPIC_AUTH_TOKEN")
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-window-side 'right))


(use-package eca
  :disabled t
  :straight (:type git :host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))

(use-package gptel
  :straight (:host github :repo "karthink/gptel")
  :config
  (require 'gptel-integrations)
  (require 'gptel-transient))

(use-package gptel-commit
  :straight (:type git :host github :repo "lakkiy/gptel-commit")
  :after (gptel magit)
  :custom
  (gptel-commit-stream t)
  (gptel-commit-use-claude-code t)
  (gptel-commit-prompt
      "You are an expert at writing Git commits. Your job is to write a short clear commit message that summarizes the changes.

If you can accurately express the change in just the subject line, don't include anything in the message body. Only use the body when it is providing *useful* information.

Don't repeat information from the subject line in the message body.

Only return the commit message in your response. Do not include any additional meta-commentary about the task. Do not include the raw diff output in the commit message.

You can take inspiration from how linux kernel project does git commits

You should not list down the changed files individually. That's what we use git for.

Follow good Git style:

- Separate the subject from the body with a blank line
- Try to limit the subject line to 50 characters
- Capitalize the subject line
- Do not end the subject line with any punctuation
- Use the imperative mood in the subject line
- Wrap the body at 72 characters
- Keep the body short and concise (omit it entirely if not useful)")
  :config
  (with-eval-after-load 'magit
    (define-key git-commit-mode-map (kbd "C-c g") #'gptel-commit)
    (define-key git-commit-mode-map (kbd "C-c G") #'gptel-commit-rationale)))

(use-package gptel-forge
  :disabled t
  :straight (:type git :host github :repo "ArthurHeymans/gptel-forge")
  :after (gptel forge)
  :config (gptel-forge-install))

(use-package opencode
  :disabled t
  :straight (:type git :host codeberg :repo "sczi/opencode.el"))

(use-package amp
  :straight (:type git :host github :repo "shaneikennedy/amp.el"))

(use-package acp :straight (:type git :host github :repo "xenodium/acp.el"))

(use-package agent-shell
  :straight (:type git :host github :repo "xenodium/agent-shell" :files ("*.el"))
  :commands (agent-shell)
  :init
  (setq agent-shell-file-completion-enabled t)
  (setq agent-shell-show-welcome-message nil)
  :config

  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)

  (add-hook 'diff-mode-hook
            (lambda ()
              (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
                (evil-emacs-state))))

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t))

  ;; env vars for codex/openai subprocesses
  (setq agent-shell-openai-codex-environment
        (agent-shell-make-environment-variables :inherit-env t)))

(use-package agent-shell-manager
  :straight (:type git :host github :repo "jethrokuan/agent-shell-manager")
  :commands (agent-shell-manager-toggle))

(defun khz/agent-shell-project-root (dir)
  "Run agent-shell in DIR (project root directory)."
  (interactive "D")
  (let ((default-directory dir))
    (call-interactively #'agent-shell)))

(with-eval-after-load 'embark
  (define-key embark-file-map (kbd "a") #'my/agent-shell-project-root))

(provide 'init-ai)
;;; init-ai.el ends here
