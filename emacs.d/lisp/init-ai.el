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

  ;; Start *agent-shell-diff* buffers to start in Emacs state
  (add-hook 'diff-mode-hook
            (lambda ()
              (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
                (evil-emacs-state))))

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t))

  ;; env vars for codex/openai subprocesses
  (setq agent-shell-openai-codex-environment
        (agent-shell-make-environment-variables :inherit-env t)))

(use-package agent-review
  :straight (:type git :host github :repo "nineluj/agent-review" :files ("*.el")))

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

(use-package ai-code
  :straight (:host github :repo "tninja/ai-code-interface.el") ;; if you want to use straight to install, no need to have MELPA setting above
  :config
  ;; use codex as backend, other options are 'claude-code, 'gemini, 'github-copilot-cli, 'opencode, 'grok, 'cursor, 'kiro, 'codebuddy, 'aider, 'claude-code-ide, 'claude-code-el
  (ai-code-set-backend 'codex)
  ;; Enable global keybinding for the main menu
  (global-set-key (kbd "C-c a") #'ai-code-menu)
  ;; Optional: Enable @ file completion in comments and AI sessions
  ;; (ai-code-prompt-filepath-completion-mode 1) ;; this is interferring corfu minibuffer
  ;; Optional: Ask AI to run test after code changes, for a tighter build-test loop
  (setq ai-code-auto-test-type 'test-after-change)
  ;; Optional: In AI session buffers, SPC in Evil normal state triggers the prompt-enter UI
  (with-eval-after-load 'evil (ai-code-backends-infra-evil-setup))
  ;; Optional: Turn on auto-revert buffer, so that the AI code change automatically appears in the buffer
  (global-auto-revert-mode 1)
  (setq auto-revert-interval 1) ;; set to 1 second for faster update
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

(use-package pi-coding-agent
  :straight (:type git :host github :repo "dnouri/pi-coding-agent")
  :if (or (executable-find "pi-coding-agent")
          (executable-find "pi"))
  :init (defalias 'pi 'pi-coding-agent))


(provide 'init-ai)
;;; init-ai.el ends here
