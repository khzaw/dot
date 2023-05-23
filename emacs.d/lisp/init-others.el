
(use-package chatgpt
  :straight (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el"))
  :init
  (require 'python)
  (setq chatgpt-repo-path (expand-file-name "straight/repos/ChatGPT.el/" user-emacs-directory))
  :bind ("C-c q" . chatgpt-query))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(provide 'init-others)
