
(use-package chatgpt
  :straight (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el"))
  :init
  (require 'python)
  (setq chatgpt-repo-path (expand-file-name "straight/repos/ChatGPT.el/" user-emacs-directory))
  :bind ("C-c q" . chatgpt-query))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (defun khz/nov-font-setup()
    (face-remap-add-relative 'variable-pitch :family "Vollkorn" :height 1.2)))

(use-package nov-xwidget
  :straight (:type git :host github :repo "chenyanming/nov-xwidget")
  :after nov
  :config
  (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
  (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))

(use-package emojify :config (global-emojify-mode))

(use-package calibredb)

(provide 'init-others)
