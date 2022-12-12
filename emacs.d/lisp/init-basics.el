;;; package --- Summary
;;; Commentary:
;;; Code:

;; Stop creating backup~ files
(setq make-backup-files nil)

;; Unless something is exploding, I do not care.
(setq warning-minimum-level :emergency)

;; Stop creating #autosave# files
(setq auto-save-default nil)

;; Stop creating .# files
(setq create-lockfiles nil)

(setq inhibit-compacting-font-caches t)

;; No startup message and screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
(setq initial-buffer-choice nil)

;; No frame title
(setq frame-title-format nil)

(setq frame-resize-pixelwise t)

;; Prevent any attempts to resize the frame.
(setq frame-inhibit-implied-resize t)

;; No file dialog
(setq use-file-dialog nil)

;; No bell
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
(setq pop-up-windows nil)

;; User name
(setq user-full-name "Kaung Htet")

;; User mail address
(setq user-mail-address "kayhzaw@gmail.com")

;; No empty line indicators
(setq indicate-empty-lines nil)

;; No cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; Text mode is initial mode
(setq initial-major-mode 'text-mode)

;; Text mode is default major mode
(setq default-major-mode 'text-mode)

;; No line break space points
(setq auto-fill-mode nil)

;; Fill column at 80
(setq fill-column 80)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; Mouse active in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; No scroll bars
(scroll-bar-mode 0)

;; No toolbar
(tool-bar-mode 0)

;; No menu bar
(if (display-graphic-p)
    (menu-bar-mode t) ;; When nil, focus problem on OSX
  (menu-bar-mode -1))

;; Navigate windows using shift+direction
(windmove-default-keybindings)

;; Enable indentation + completion using the TAB key
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Better scrolling
(setq scroll-margin 10
  scroll-step 1
  next-line-add-newlines nil
  scroll-conservatively 10000
  scroll-preserve-screen-position t
  auto-window-vscroll nil)


(blink-cursor-mode t)
;; blinks cursor forever
(setq blink-cursor-blinks 0)

;; Linux specific
(when (eq system-type 'gnu/linux)
  (setq x-super-keysym 'meta))

;; Mac specific
(when (eq system-type 'darwin)
  ;; Don't use native fullscreen
  (setq ns-use-native-fullscreen nil
    ;; Both command keys are super
    ;; mac-command-modifier 'meta
    mac-right-command-modifier 'super
    ;; Option is meta
    mac-option-modifier 'meta
    ;; Right Alt is not meta, used to type symbols and shit
    mac-right-option-modifier 'meta))

;; Default shell in term
(when (eq system-type 'gnu/linux)
  (setq-default shell-file-name "/usr/bin/zsh")
  (setq explicit-shell-file-name "/usr/bin/zsh"))

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Tab.space equivalence
(setq-default tab-width 2)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; Follow symlinks for vc
(setq vc-follow-symlinks t)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Revert (updatet) buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Kill term buffer when exiting
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(use-package restart-emacs)

(use-package which-key
  :diminish
  :init
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.1)
  (setq which-key-allow-multiple-replacements t)
  :config
  (push '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1")) which-key-replacement-alist)
  (push '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1")) which-key-replacement-alist)
  (which-key-mode)
  (setq which-key-unicode-correction 5))

(use-package which-key-posframe
  :after (posframe which-key)
  :config (setq which-key-posframe-border-width 15))

(use-package rg)

(use-package editorconfig
  :defer 1
  :diminish
  :config (editorconfig-mode 1)
  :custom (editorconfig-lisp-use-default-indent t))

;; History
(use-package saveplace
  :straight (:type built-in)
  :hook (after-init . save-place-mode))

(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
          recentf-exclude
          '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
             "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
             "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
             "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
             (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package time
  :straight (:type built-in)
  :init (setq display-time-24hr-format t))

(use-package so-long
  :hook (after-init . global-so-long-mode))

;; Better sorting
(use-package prescient
  :commands prescient-persist-mode
  :init (prescient-persist-mode 1)
  :config
  (setq prescient-sort-full-matches-first t
    prescient-sort-length-enable t))


(provide 'init-basics)
;;; init-basics.el ends here
