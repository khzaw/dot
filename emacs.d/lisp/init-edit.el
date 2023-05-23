;; Automatically reload files modified by external program  -*- lexical-binding: t; -*-
(use-package autorevert
  :straight (:type built-in)
  :diminish
  :hook (after-init . global-auto-revert-mode))

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
  :bind (("C-,"   . avy-goto-char)
          ("C-'"   . avy-goto-char-2)
          ("M-g g" . avy-goto-line)
          ("M-g w" . avy-goto-word-0)
          ("M-g e" . avy-goto-word-1))
  :hook (after-init . avy-setup-default)
  :config (setq avy-background t
            avy-all-windows nil
            avy-all-windows-alt t
            avy-style 'pre))

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

(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
    undo-tree-enable-undo-in-region nil
    undo-tree-auto-save-history nil))

;; Narrow/Widen
(use-package fancy-narrow
  :diminish
  :hook (after-init . fancy-narrow-mode))

;; On-the-fly spell checker
(use-package flyspell
  :straight (:type built-in)
  :diminish
  :if (executable-find "aspell")
  :bind (("C-c C-x s" . flyspell-correct-word))
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init (setq flyspell-issue-message-flag nil
              ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

(use-package jinx
  :after (vertico vertico-multiform)
  :straight (jinx :type git :host github :repo "minad/jinx" :files (:defaults "jinx-mod.c" "emacs-module.h"))
  :hook ((emacs-startup-hook . global-jinx-mode))
  :bind (([remap ispell-word] . #'jinx-correct))
  :config
  (vertico-multiform-mode 1))


(use-package highlight-indent-guides
  :diminish
  :hook
  (yaml-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-character ?\xFFE8)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-method 'character))

(use-package undo-fu)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package writeroom-mode
  :bind (("C-c w r" . writeroom-mode))
  :config
  (setq writeroom-width 100
    writeroom-fullscreen-effect nil
    writeroom-maximize-window nil))

(use-package olivetti
  :hook ((text-mode . olivetti-mode)
          ;; (prog-mode . olivetti-mode)
          (org-mode . olivetti-mode)
          (helpful-mode . olivetti-mode)
          (fundamental-mode . olivetti-mode)
          (elfeed-show-mode . olivetti-mode)
          (Info-mode . olivetti-mode)
          (markdown-mode . olivetti-mode))
  :custom
  ;; (olivetti-margin-width 12)
  (olivetti-body-width 120)
  (olivetti-style 'fancy)
  :delight "⊛")

(use-package apheleia
  :config
  ;; (apheleia-global-mode t)
  )

(use-package repeat-mode
  :straight (:type built-in)
  :hook (after-init . repeat-mode))

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

(provide 'init-edit)
;;; init-edit.el ends here
