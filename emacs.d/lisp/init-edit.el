;; Automatically reload files modified by external program  -*- lexical-binding: t; -*-
(use-package autorevert
  :straight (:type built-in)
  :diminish
  :hook (on-first-buffer . global-auto-revert-mode))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
          ("C--" . er/contract-region)
          ("C-(" . er/mark-outside-pairs)))

(use-package symbol-overlay
  :diminish
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("M-n" . symbol-overlay-jump-next)
          ("M-p" . symbol-overlay-jump-prev)))

(use-package avy
  :bind (("M-g c" . avy-goto-char)
         ("M-g C" . avy-goto-char-2)
         ;; ("C-,"   . avy-goto-char)
         ("C-'"   . avy-goto-char-2)
         ("M-g g" . avy-goto-line)
         ("M-g w" . avy-goto-word-0)
         ("M-g s" . avy-goto-subword-1))
  :hook (after-init . avy-setup-default)
  :config (setq avy-background t
                avy-all-windows nil
                avy-all-windows-alt t
                avy-style 'pre))

(use-package lasgun
  :straight (:type git :host github :repo "aatmunbaxi/lasgun.el")
  :after (avy embark))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((clojure-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)))

(use-package symbol-overlay
  :defer t
  :commands (symbol-overlay-mode symbol-overlay-put))


;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=55340
(defun fix-electric-indent ()
  "Honour `electric-pair-open-newline-between-pairs'.
  Member of `post-self-insert-hook' if `electric-pair-mode' is on."
  (when (and (if (functionp electric-pair-open-newline-between-pairs)
               (funcall electric-pair-open-newline-between-pairs)
               electric-pair-open-newline-between-pairs)
          (eq last-command-event ?\n)
          (< (1+ (point-min)) (point) (point-max))
          (eq (save-excursion
                (skip-chars-backward "\t\s")
                (char-before (1- (point))))
            (matching-paren (char-after))))
    (save-excursion (newline-and-indent))))
(advice-add 'electric-pair-open-newline-between-pairs-psif :override #'fix-electric-indent)

;; Automatic parenthesis pairing
(use-package elec-pair
  :straight (:type built-in)
  :hook (after-init . electric-pair-mode))

(use-package vundo
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-auto-save-history nil))

;; Narrow/Widen
(use-package fancy-narrow
  :diminish
  :hook (after-init . fancy-narrow-mode))

(use-package undo-fu
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :custom
  ;; 3 times the default values
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000)))

(use-package undo-fu-session
  :config (undo-fu-session-global-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package writeroom-mode
  :bind
  (("C-c w r" . writeroom-mode)
   (:map writeroom-mode-map
    ("C-M-<" . writeroom-decrease-width)
    ("C-M->" . writeroom-increase-width)
    ("C-M-=" . writeroom-adjust-width)))
  :config
  (setq writeroom-width 140
        writeroom-fullscreen-effect nil
        writeroom-maximize-window nil
        writeroom-major-modes '(text-mode prog-mode helpful-mode
                                          forge-pullreq-mode
                                          magit-status-mode magit-diff-mode
                                          magit-log-mode magit-stash-mode
                                          magit-revision-mode magit-process-mode)
        writeroom-mode-line-toggle-position 'mode-line-format
        writeroom-mode-line t)
  :init
  (global-writeroom-mode 1))

(use-package olivetti
  :straight (:type git :host github :repo "rnkn/olivetti")
  ;; :hook (org-mode . olivetti-mode)
  :custom
  (olivetti-margin-width 12)
  (olivetti-body-width 100)
  (olivetti-style 'fancy))

;; (use-package auto-olivetti
;;   :straight (auto-olivetti :host sourcehut :repo "ashton314/auto-olivetti")
;;   :config
;;   (auto-olivetti-mode))

(use-package apheleia
  ;; :config
  ;; (apheleia-global-mode t)
  )

(use-package repeat-mode
  :straight (:type built-in)
  :hook (after-init . repeat-mode))

(use-package repeat-help
  :straight (:type git :host github :repo "karthink/repeat-help")
  :hook (repeat-mode . repeat-help-mode))


(use-package hippie-exp
  :straight (:type built-in)
  :bind ("M-/" . hippie-expand)
  :init
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-visible
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package coverlay)

(use-package indent-tools
  :bind ("C-c TAB" . indent-tools-hydra/body))

(use-package indent-bars
  :disabled t
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook ((python-mode yaml-mode) . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement if_statement with_statement while_statement)))
  ;; wrap may not be needed if no-descend-list is enough
  ;; (indent-bars-treesit-wrap '((python argument_list parameters ; for python , as an example list list_comprehension
  ;; dictionary dictionary_comprehension paranthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode) . indent-bars-mode)
  :config
  (setq indent-bars-color '(highlight :face-bg t :blend 0.2)
        indent-bars-pattern "."
        indent-bars-width-frac 0.1
        indent-bars-pad-frac 0.1
        indent-bars-zigzag nil
        indent-bars-color-by-depth nil
        indent-bars-highlight-current-depth nil
        indent-bars-display-on-blank-lines nil))

(use-package anzu
  :config
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-anzu-mode +1))

(use-package ipe
  :straight (:type git :host github :repo "BriansEmacs/insert-pair-edit.el")
  :config
  (global-set-key [remap insert-parentheses] 'ipe-insert-pair-edit)
  (global-set-key (kbd "A-(") 'ipe-insert-pair-edit-update)
  (global-set-key (kbd "H-(") 'ipe-insert-pair-edit-delete)
  (require 'ipe-markdown-mode)
  (require 'ipe-texinfo-mode)
  (require 'ipe-html-mode))

(use-package string-inflection)

(use-package vlf
  ;; very large files
  :config (require 'vlf-setup))

(provide 'init-edit)
;;; init-edit.el ends here
