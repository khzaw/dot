(defun corfu-beginning-of-prompt ()
  "Move to end of completion input."
  (interactive)
  (corfu--goto -1)
  (goto-char (car completion-in-region--data)))

(defun corfu-end-of-prompt ()
  "Move to end of completion input."
  (interactive)
  (corfu--goto -1)
  (goto-char (cadr completion-in-region--data)))

(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
         completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))

(use-package tempel
  :bind (("M-+" . tempel-complete)
          ("M-*" . tempel-insert))
  :init
  ;; setup completion at point
  (defun tempel-setup-caf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: we add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
      (cons #'tempel-expand
        completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

(use-package corfu
  :init
  (global-corfu-mode)
  :bind (:map corfu-map
          ("M-m" . corfu-move-to-minibuffer)
          ([remap move-beginning-of-line] . corfu-beginning-of-prompt)
          ([remap move-end-of-line] . corfu-end-of-prompt)))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :bind (("M-p p" . completion-at-point) ;; capf
          ("M-p t" . complete-tag)        ;; etags
          ("M-p d" . cape-dabbrev)        ;; or dabbrev-completion
          ("M-p h" . cape-history)
          ("M-p f" . cape-file)
          ("M-p k" . cape-keyword)
          ("M-p s" . cape-symbol)
          ("M-p a" . cape-abbrev)
          ("M-p i" . cape-ispell)
          ("M-p l" . cape-line)
          ("M-p w" . cape-dict)
          ("M-p \\" . cape-tex)
          ("M-p _" . cape-tex)
          ("M-p ^" . cape-tex)
          ("M-p &" . cape-sgml)
          ("M-p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))

(provide 'init-corfu)
