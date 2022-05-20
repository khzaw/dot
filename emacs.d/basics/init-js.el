;;; package --- init-js.el
;;; Commentary:

;;; Code:
(use-package rjsx-mode
  :hook (rjsx-mode . (lambda ()
                       (flycheck-mode)
                       (company-mode))))
(use-package prettier
  :after (add-node-modules-path)
  :hook (solidity-mode . prettier-mode)
  (rjsx-mode . prettier-mode)
  (js2-mode . prettier-mode))

;; (use-package prettier-js
;;   :hook ((rjsx-mode . prettier-js-mode)
;;           (solidity-mode . prettier-js-mode)
;;           (js2-mode . prettier-js-mode)))

(use-package add-node-modules-path
  :hook ((rjsx-mode . add-node-modules-path)
          (js2-mode . add-node-modules-path)))

(provide 'init-js)
;;; init-js.el ends here
