(use-package solidity-mode
  :config
  (setq solidity-comment-style 'slash)
  (setq solidity-solc-path "/usr/local/bin/solc")
  :bind
  (:map solidity-mode-map
    ("C-c s g" . solidity-estimate-gas-at-point)))

(use-package solidity-flycheck
  :after (solidity-mode flycheck))

(use-package company-solidity
  :after (solidity-mode company))

(provide 'init-solidity)
;;; init-solidity.el ends here
