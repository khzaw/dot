(defun khz/org-present-hook ()
  (org-present-big)
  (org-display-inline-images)
  (setq visual-fill-column-width 110)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (evil-emacs-state))

(defun khz/org-present-quit-hook ()
  "Configurations to run when `org-present-mode' ends."
  ;; Reset font
  (setq-local face-mapping-alist nil)
  (org-present-small)
  (org-present-show-cursor)
  (org-present-read-write)
  (visual-fill-column-mode 0)
  (evil-normal-state))

(use-package org-present
  :commands (org-present)
  :hook ((org-present-mode . khz/org-present-hook)
         (org-present-mode-quit . khz/org-present-quit-hook)))

(provide 'init-presentation)
;;; init-presentation.el ends here
