(use-package hide-mode-line)


(defun khz/org-present-start ()
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun khz/org-present-end ()
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

(use-package org-present
  :init
  (setq visual-fill-column-width 110)
  (setq visual-fill-column-center-text t)
  :hook ((org-present-mode . khz/org-present-start)
          (org-present-quit . khz/org-present-end)))

(provide 'init-presentation)
;;; init-presentation.el ends here
