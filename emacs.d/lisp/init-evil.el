;; -*- lexical-binding: t; -*-

(use-package evil
  :preface (setq evil-want-keybinding nil)
  :init
  (setq evil-undo-system 'undo-fu)
  :config
  (setq evil-want-integration t
        evil-want-minibuffer nil
        evil-mode-line-format nil
        evil-kill-on-visual-paste nil
        evil-symbol-word-search t
        evil-respect-visual-line-mode t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-want-C-u-scroll nil
        evil-ex-interactive-search-highlight 'selected-window
        evil-kbd-macro-suppress-motion-error t)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'fundamental-mode 'emacs)
  (evil-set-initial-state 'chronos-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'symbols-outline-mode 'emacs)
  (evil-mode 1))

(use-package evil-escape
  :straight (evil-escape :type git :host github :repo "syl20bnr/evil-escape")
  :diminish
  :init
  (setq-default evil-escape-key-sequence "kj")
  :config
  (evil-escape-mode))

(use-package evil-collection
  :after (evil magit)
  :config
  (evil-collection-define-key 'normal 'emacs-lisp-mode-map "K" 'helpful-at-point)
  ;; (setq evil-collection-outline-bind-tab-p t) ;; enable <tab>-based bindings in Outline mode.
  ;; q is enough; ESC is way too easy to accidentally press
  (evil-collection-init)
  (diminish 'evil-collection-unimpaired-mode))

(use-package evil-leader
  :commands (evil-leader-mode)
  :init (global-evil-leader-mode)
  :config
  (progn
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "f" 'isearch-forward
      "F" 'isearch-backward
      "g s" 'magit-status
      "b" 'consult-buffer)))

(use-package evil-surround
  :commands global-evil-surround-mode
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))

     (?\) . ("(" . ")"))
     (?\] . ("[" . "]"))
     (?\} . ("{" . "}"))

     (?< . ("<" . ">"))
     (?> . ("<" . ">"))))
  :hook (after-init . global-evil-surround-mode))

(use-package evil-goggles :config (evil-goggles-mode))

;; provides 2-character motions for quickly jumping around text compared to Evil's built-in f/F/t/T motions, incrementally highlighting candidate targets as you type. By default, snipe only binds s (forward) and S (backward) to evil-snipe-s and evil-snipe-S, respectively. In operator mode, snipe is bound to z/Z and x/X (exclusive):
(use-package evil-snipe
  :commands evil-snipe-mode
  :hook (after-init . evil-snipe-mode))

;; enable commenting and uncommenting by pressing gcc in normal mode and gc in visual mode
(with-eval-after-load "evil"
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))

(use-package evil-lion :config (evil-lion-mode))

;; Evil commentary
(use-package evil-commentary
  :config (evil-commentary-mode))

;; Evil text objects
(use-package evil-textobj-line)
(use-package evil-textobj-syntax)
(use-package evil-indent-plus
  :config
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up-down))

(use-package evil-paredit
  :disabled t
  :after (paredit evil)
  :hook ((emacs-lisp-mode . evil-paredit-mode)
         (scheme-mode . evil-paredit-mode)
         (racket-mode . evil-paredit-mode)
         (lisp-mode . evil-paredit-mode)))

(provide 'init-evil)
;;; init-evil.el ends here
