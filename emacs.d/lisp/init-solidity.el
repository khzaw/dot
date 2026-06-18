;; -*- lexical-binding: t; -*-
(use-package solidity-mode
  :bind (:map solidity-mode-map
         ("C-c s g" . solidity-estimate-gas-at-point))
  :mode ("\\.sol\\'")
  :config
  (setq solidity-comment-style 'slash)
  (with-eval-after-load 'eglot
    (add-hook 'solidity-mode-hook 'eglot-ensure)))

(use-package solidity-flycheck
  :after (solidity-mode flycheck))

(use-package company-solidity
  :after (solidity-mode company))

(provide 'init-solidity)
;;; init-solidity.el ends here
