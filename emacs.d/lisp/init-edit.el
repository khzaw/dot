;; -*- lexical-binding: t; -*-

(delete-selection-mode 1) ; replace selected text with typed text

(use-package autorevert
  :straight (:type built-in)
  :diminish
  :hook (on-first-buffer . global-auto-revert-mode))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)
         ("C-(" . er/mark-outside-pairs)
         (:map evil-visual-state-map
          ("v" . er/expand-region)
          ("V" . er/contract-region)))
  :config
  (defun treesit-mark-bigger-node ()
    "Use tree-sitter to mark regions"
    (let* ((root (treesit-buffer-root-node))
           (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
           (node-start (treesit-node-start node))
           (node-end (treesit-node-end node)))
      ;; Node fits the region exactly. Try its parent node instead.
      (when (and (= (region-beginning) node-start) (= (region-end) node-end))
        (when-let* ((node (treesit-node-parent node)))
          (setq node-start (treesit-node-start node)
                node-end (treesit-node-end node))))
      (set-mark node-end)
      (goto-char node-start)))
  (add-to-list 'er/try-expand-list 'treesit-mark-bigger-node))

(use-package symbol-overlay
  :diminish
  :hook (prog-mode . symbol-overlay-mode)
  :bind-keymap ("M-s M-s" . symbol-overlay-map)
  :bind (:map symbol-overlay-mode-map
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev))
  :commands (symbol-overlay-mode symbol-overlay-put))

(use-package symbol-overlay-mc
  :straight (:type git :host github :repo "xenodium/symbol-overlay-mc")
  :config
  (with-eval-after-load 'casual-symbol-overlay
    (symbol-overlay-mc-insert-into-casual-tmenu)))

(use-package smartscan
  :disabled t
  :config (smartscan-mode))

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

(use-package aggressive-indent
  ;; Minor mode to aggressively keep your code always indented
  :diminish
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :config (setq aggressive-indent-region-function #'aggressive-indent-indent-defun))

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

;; (advice-add 'indent-region :around
;;             (lambda (orig-fun &rest args)
;;               (let ((inhibit-message t)) ; Suppress echo area messages
;;                 (apply orig-fun args)))
;;             '((name . "silence-indent-region")))

;; Automatic parenthesis pairing
(use-package elec-pair
  :straight (:type built-in)
  ;; :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :hook ((after-init . electric-pair-mode)
         (org-mode . (lambda ()
                       (setq-local electric-pair-inhibit-predicate
                                   `(lambda (c)
                                      (if (char-equal c ?<) t
                                        (,electric-pair-inhibit-predicate c)))))))
  :config
  ;; https://www.reddit.com/r/emacs/comments/1hwf46n/comment/m63mddk
  ;; This ensures multiple quotes are not added at the beginning or end of a word
  ;; https://github.com/meain/dotfiles/blob/81ecc82265d4b4c59bc742015c6ba7502b30299a/emacs/.config/emacs/init.el#L366C3-L383C83
  (defun khz/electric-pair-conservative-inhibit (char)
    (or
     ;; I find it more often preferable not to pair when the
     ;; same char is next.
     (eq char (char-after))
     ;; Don't pair up when we insert the second of "" or of ((.
     (and (eq char (char-before))
          (eq char (char-before (1- (point)))))
     ;; I also find it often preferable not to pair next to a word.
     (eq (char-syntax (following-char)) ?w)
     ;; Don't pair at the end of a word, unless parens.
     (and
      (eq (char-syntax (char-before (1- (point)))) ?w)
      (eq (preceding-char) char)
      (not (eq (char-syntax (preceding-char)) ?\()))))
  (setq electric-pair-inhibit-predicate 'khz/electric-pair-conservative-inhibit))

;; Visual `align-regexp'
(use-package ialign)

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
  :hook (prog-mode . hl-todo-mode)
  :bind (:map hl-todo-mode-map
         ("C-! h p" . hl-todo-previous)
         ("C-! h n" . hl-todo-next)
         ("C-! h o" . hl-todo-occur)
         ("C-! h r" . hl-todo-rg-project)
         ("C-! h R" . hl-todo-rg))
  :config

  (defun hl-todo-rg (regexp &optional files dir)
    "Use `rg' to find all TODO or similar keywords."
    (interactive
     (progn
       (unless (require 'rg nil t)
         (error "`rg' is not installed"))
       (let ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
         (list regexp
               (rg-read-files)
               (read-directory-name "Base directory: " nil default-directory t)))))
    (rg regexp files dir))

   (defun hl-todo-rg-project ()
    "Use `rg' to find all TODO or similar keywords in current project."
    (interactive)
    (unless (require 'rg nil t)
      (error "`rg' is not installed"))
    (rg-project (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp)) "everything")))

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
        writeroom-mode-line t))

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
  :diminish
  :hook (after-init . repeat-mode)
  :config (setq repeat-message-function #'ignore))

(use-package repeat-help
  :diminish
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
  :commands (indent-bars-mode)
  :hook ((python-mode . indent-bars-mode)
         (python-ts-mode . indent-bars-mode)
         (yaml-mode . indent-bars-mode)
         (yaml-ts-mode . indent-bars-mode))
  :custom
  (indent-bars-treesit-support t)

  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  ;; (indent-bars-treesit-scope '((python function_definition class_definition for_statement if_statement with_statement while_statement)))
  ;; wrap may not be needed if no-descend-list is enough
  ;; (indent-bars-treesit-wrap '((python argument_list parameters ; for python , as an example list list_comprehension
  ;; dictionary dictionary_comprehension paranthesized_expression subscript)))
  :config
  (setq indent-bars-prefer-character nil)
  ;; (setq indent-bars-color '(highlight :face-bg t :blend 0.2)
  ;;       indent-bars-pattern "."
  ;;       indent-bars-width-frac 0.1
  ;;       indent-bars-pad-frac 0.1
  ;;       indent-bars-zigzag nil
  ;;       indent-bars-color-by-depth nil
  ;;       indent-bars-highlight-current-depth nil
  ;;       indent-bars-display-on-blank-lines nil)
  )

(use-package anzu
  :diminish
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

(use-package dtrt-indent
  :ensure t
  :commands (dtrt-indent-global-mode
             dtrt-indent-mode
             dtrt-indent-adapt
             dtrt-indent-undo
             dtrt-indent-diagnosis
             dtrt-indent-highlight)
  :config
  (dtrt-indent-global-mode))

(use-package transform-symbol-at-point
  :straight (:type git :host github :repo "waymondo/transform-symbol-at-point")
  :custom (transform-symbol-at-point-cursor-after-transform 'next-symbol)
  :bind ("s-;" . transform-symbol-at-point))

(use-package smartparens
  :diminish
  :straight (smartparens :type git
                         :host github
                         :repo "Fuco1/smartparens")
  :hook (prog-mode text-mode markdown-mode)
  :config (require 'smartparens-config)
  :commands (smartparens-mode sp-forward-slurp-sexp))

(use-package goto-chg)

(use-package outline-indent
  :ensure t
  :commands (outline-indent-minor-mode
             outline-indent-insert-heading)
  :custom
  (outline-indent-ellipsis " ▼ ")
  :hook
  ((python-mode . outline-indent-minor-mode)
   (python-ts-mode . outline-indent-minor-mode)
   (yaml-mode . outline-indent-minor-mode)
   (yaml-ts-mode . outline-indent-minor-mode)))

(use-package ov
  :straight (:type git :host github :repo "emacsorphanage/ov"))

(use-package dumb-jump
  :config
  (setq dumb-jump-prefer-searcher 'rg
        xref-history-storage #'xref-window-local-history
        xref-show-definitions-function #'xref-show-definitions-completing-read)

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; Do not use the etags backend.
  (remove-hook 'xref-backend-functions #'etags--xref-backend))

(use-package emacs
  :straight (:type built-in)
  :general (:states 'normal
                    "g ." '("find def"       . xref-find-definitions)
                    "g >" '("find def o/win" . xref-find-definitions-other-window)
                    "g ," '("def go back"    . xref-go-back)
                    "g <" '("def go forward" . xref-go-forward)
                    "g /" '("find refs"      . xref-find-references)
                    "g ?" '("find/rep refs"  . xref-find-references-and-replace)
                    "g h" '("find apropos"   . xref-find-apropos)
                    "g b" '("def go back"    . xref-go-back)))

;; (use-package ws-butler
;;   :straight (:type git :host github :repo "lewang/ws-butler"))

(provide 'init-edit)
;; init-edit.el ends here
