;; -*- lexical-binding: t; -*-

(use-package shell-maker
  :straight (:type git :host github :repo "xenodium/shell-maker"))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-window-side 'right))

(use-package agent-shell
  :straight (:type git :host github :repo "xenodium/agent-shell"))

(use-package eca
  :disabled t
  :straight (:type git :host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))

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

(provide 'init-ai)
;;; init-ai.el ends here
