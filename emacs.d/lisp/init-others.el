(use-package gptel
  :straight (gptel :type git :host github :repo "karthink/gptel"))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (defun khz/nov-font-setup()
    (face-remap-add-relative 'variable-pitch :family "Vollkorn" :height 1.2)))

(use-package nov-xwidget
  :straight (:type git :host github :repo "chenyanming/nov-xwidget")
  :after nov
  :hook (nov-mode . nov-xwidget-inject-all-files)
  :config
  (define-key nov-mode-map (kbd "o") 'nov-xwidget-view))

(use-package emojify)

(use-package calibredb)

(use-package bookmark-view
  :straight (bookmark-view :type git :host github :repo "minad/bookmark-view"))


;; (use-package mugur
;;  :straight (mugur :type git :host github :repo "mihaiolteanu/mugur"))

(use-package devdocs
  :straight (:type git :host github :repo "astoff/devdocs.el")
  :bind ("C-h D" . devdocs-lookup))

(global-set-key (kbd "C-c C-'") 'uuidgen)

(provide 'init-others)
