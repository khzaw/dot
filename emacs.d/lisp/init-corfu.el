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
