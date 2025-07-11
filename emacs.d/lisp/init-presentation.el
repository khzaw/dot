;; -*- lexical-binding: t; -*-

(defvar khz/original-font-height nil
  "Store the original font height before presentation mode.")

(defvar khz/original-org-hide-block-startup nil
  "Store the original org-hide-block-startup value.")

(defun khz/org-present-hook ()
  (setq khz/original-font-height (face-attribute 'default :height))
  (setq khz/original-org-hide-block-startup org-hide-block-startup)


  (set-face-attribute 'default nil :height 250)
  (setq org-hide-block-startup t)
  (org-block-map (lambda () (org-hide-block-toggle)))
  (org-present-big)
  (org-display-inline-images)
  (setq visual-fill-column-width 110)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (evil-emacs-state))

(defun khz/org-present-quit-hook ()
  "Configurations to run when `org-present-mode' ends."

  (when khz/original-font-height
    (set-face-attribute 'default nil :height khz/original-font-height))

  (when khz/original-org-hide-block-startup
    (setq org-hide-block-startup khz/original-org-hide-block-startup))
  (org-block-map (lambda () (org-show-block-all)))

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
         (org-present-mode-quit . khz/org-present-quit-hook))
  :custom (org-present-startup-folded nil)
  (org-present-hide-stars-in-headings nil))

(use-package dslide
    :straight (dslide :type git :host github
                      :repo "positron-solutions/dslide"))

(provide 'init-presentation)
;;; init-presentation.el ends here
