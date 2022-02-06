;;; package --- init-prog.el
;;; Commentary:

;;; Code:
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode))

(provide 'init-prog)
;;; init-prog.el ends here
