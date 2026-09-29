;; -*- lexical-binding: t; -*-

(use-package shell-maker
  :straight (:type git :host github :repo "xenodium/shell-maker")
  :defer t)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-window-side 'right))

(use-package eca
  :disabled t
  :straight (:type git :host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))

(use-package gptel
  :straight (:host github :repo "karthink/gptel")
  :commands (gptel gptel-send gptel-menu gptel-mode gptel-rewrite)
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
  :straight (:type git :host github :repo "shaneikennedy/amp.el")
  :defer t)

(use-package acp
  :straight (:type git :host github :repo "xenodium/acp.el")
  :defer t)

(use-package agent-shell
  :straight (:type git :host github :repo "xenodium/agent-shell" :files ("*.el"))
  :commands (agent-shell)
  :custom
  (agent-shell-file-completion-enabled t)
  (agent-shell-show-welcome-message nil)
  ;; Offer every configured agent, with Codex preselected rather than forced.
  (agent-shell-preferred-agent-config '(preselect . codex))
  (agent-shell-session-strategy 'prompt)
  ;; ~/.bin/codex-acp is older than the Codex CLI/config.  Resolve the
  ;; mise-managed adapter explicitly rather than whichever PATH finds first.
  (agent-shell-openai-codex-acp-command
   '("mise" "exec" "npm:@agentclientprotocol/codex-acp" "--" "codex-acp"))
  :preface
  (defun khz/agent-shell-project-root (dir)
    "Run agent-shell in DIR."
    (interactive "D")
    (let ((default-directory dir))
      (call-interactively #'agent-shell)))
  :config
  ;; Native ACP backends: keep the picker focused on the agents we use.
  ;; Requires agent-shell with its built-in agent-shell-xai backend.
  (setq agent-shell-agent-configs
        (list (agent-shell-openai-make-codex-config)
              (agent-shell-pi-make-agent-config)
              (agent-shell-opencode-make-agent-config)
              (agent-shell-anthropic-make-claude-code-config)
              (agent-shell-xai-make-grok-config)))

  ;; Reuse CLI logins: Codex's ChatGPT subscription, OpenCode's provider
  ;; setup, and Claude's login (requires an eligible plan/account).
  ;; Pi and Grok use their own CLI credentials without an Emacs API key.
  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t)
        agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication :none t)
        agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))

  ;; acp.el inherits the environment at process launch.  Do not snapshot it:
  ;; this preserves current PATH/mise and buffer-local envrc environments.
  ;; Keep Codex on subscription auth even if a CODEX_API_KEY is inherited.
  (setq agent-shell-openai-codex-environment '("CODEX_API_KEY="))

  ;; Install missing adapters with mise (not npm -g):
  ;; mise use -g npm:pi-acp npm:@agentclientprotocol/claude-agent-acp
  ;; mise use -g npm:@agentclientprotocol/codex-acp
  ;; OpenCode and Grok already speak ACP via `opencode acp' / `grok agent stdio'.
  ;; pi-acp spawns the installed `pi --mode rpc', reusing its settings/skills.
  ;; TUI-only extensions and extension slash commands may need the Pi UI.

  ;; Keep multiline insert-state editing, but use the native submit command
  ;; so queuing/steering works instead of bypassing it via comint-send-input.
  (with-eval-after-load 'evil
    (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
    (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'agent-shell-submit)
    (evil-set-initial-state 'agent-shell-diff-mode 'emacs))

  (with-eval-after-load 'embark
    (define-key embark-file-map (kbd "a") #'khz/agent-shell-project-root)))

(use-package agent-review
  :straight (:type git :host github :repo "nineluj/agent-review" :files ("*.el"))
  :commands (agent-review agent-review-start))

(use-package agent-shell-manager
  :straight (:type git :host github :repo "jethrokuan/agent-shell-manager")
  :commands (agent-shell-manager-toggle))

(use-package ai-code
  :straight (:host github :repo "tninja/ai-code-interface.el")
  :bind ("C-c a" . ai-code-menu)
  :config
  ;; use codex as backend, other options are 'claude-code, 'gemini, 'github-copilot-cli, 'opencode, 'grok, 'cursor, 'kiro, 'codebuddy, 'aider, 'claude-code-ide, 'claude-code-el
  (ai-code-set-backend 'codex)
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
  :after markdown-table-wrap
  :if (or (executable-find "pi-coding-agent")
          (executable-find "pi"))
  :init (defalias 'pi 'pi-coding-agent)
  :hook
  (pi-coding-agent-chat-mode
   . (lambda ()
       (when (or (memq 'ivory-light custom-enabled-themes)
                 (memq 'ivory-dark custom-enabled-themes))
         (face-remap-add-relative
          'md-ts-block-quote
          `(:foreground ,(face-attribute 'font-lock-comment-face
                                          :foreground nil t)
            :slant normal)))))
  :config
  (setq package-install-upgrade-built-in t))

(provide 'init-ai)
;;; init-ai.el ends here
