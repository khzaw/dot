(use-package hide-mode-line)

;; (use-package org-faces :straight nil)

;; (dolist (face '((org-level-1 . 1.3)
;;                  (org-level-2 . 1.1)
;;                  (org-level-3 . 1.05)
;;                  (org-level-4 . 1.0)
;;                  (org-level-5 . 1.0)
;;                  (org-level-6 . 1.0)
;;                  (org-level-7 . 1.0)
;;                  (org-level-8 . 1.0))))
;;   (set-face-attribute (car face) nil :weight 'medium :height (cdr face)))

;; Make the document title a bit bigger
;; (set-face-attribute 'org-document-title nil :weight 'bold :height 1.3)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
;; (set-face-attribute 'org-block nil :foreground nil :inherit 'variable-pitch)
;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(use-package org-present
  :preface
  (defun khz/org-present-start ()
    "Configurations to run when `org-present-mode' starts."
    ;; Set a blank header line string to create blank space at the top
    (setq-local header-line-format " ")
    ;; Tweak font sizes in presentation
    (setq-local face-mapping-alist '((default (:height 1.5) default)))

    (setq-local visual-fill-column-width 120)
    (setq-local visual-fill-column-center-text t)
    (visual-fill-column-mode 1)
    (hide-mode-line-mode 1)
    (visual-line-mode 1))

  (defun khz/org-present-end ()
    "Configurations to run when `org-present-mode' ends."
    ;; Reset font
    (setq-local face-mapping-alist '((default variable-pitch default)))
    (visual-fill-column-mode 0)
    (hide-mode-line-mode 0)
    (visual-line-mode 0))

  (defun khz/org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines
    (org-overview)

    ;; Unfold the current entry
    (org-show-entry)

    ;; Show only direct subheadings of the slide but don't expand them
    (org-show-children))

  :hook ((org-present-mode . khz/org-present-start)
          (org-present-mode-quit . khz/org-present-end)
          (org-present-after-navigate-functions . khz/org-present-prepare-slide)))

(provide 'init-presentation)
;;; init-presentation.el ends here
