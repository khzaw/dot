;;; -*- lexical-binding: t -*-
(setq package-enable-at-startup nil)
;; Native compilation stuff
(when (featurep 'native-compile)
  (defvar native-comp-async-report-warnings-errors)

  (setq native-comp-speed 2
        ;; Silence compiler warnings
        native-comp-async-report-warnings-errors nil
        ;; Compile loaded packages asynchronously (renamed in Emacs 29)
        native-comp-jit-compilation t
        warning-suppress-log-types '((comp))))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Defer garbage collection further back in the startup process.
;; gcmh (loaded in init-package.el) will restore sensible values once idle.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Set-language-environment sets `default-input-method', which is unwanted.
(setq default-input-method nil)

;; Package initialization is handled by straight.el (see init-package.el).

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Set default coding system
(set-language-environment "UTF-8")

;; Faster to disable these here (before they've been initialized)
;; (push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)
;; (when (featurep 'ns)
;;   (push '(ns-transparent-titlebar . t) default-frame-alist))


;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq default-frame-alist
      '((vertical-scroll-bars . nil)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (ns-transparent-titlebar . t)))

(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(right-divider-width . 20))
  (add-to-list var '(internal-border-width . 20)))

(setq inhibit-compacting-font-caches t)

(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering  'left-to-right
              bidi-paragraph-direction 'left-to-right)

;;;; Clean View
;; UI - Disable visual cruft

;; Resizing the Emacs frame can be an expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setopt frame-inhibit-implied-resize t
        frame-resize-pixelwise t
        ;; HACK: Don't show size info (or anything else) in frame title
        frame-title-format "\n"
        ;; Disable start-up screen
        inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message (user-login-name)
        ;; We'll provide our own splash screen, thanks
        inhibit-splash-screen t
        ;; No message in initial scratch buffer
        initial-scratch-message nil)

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setopt tool-bar-mode nil
        scroll-bar-mode nil)

(defconst emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")


;; Unset `file-name-handler-alist' too (temporarily). Every file opened and
;; loaded by Emacs will run through this list to check for a proper handler for
;; the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old)))

;; For LSP mode, use plists for deserialization
;; For more info, see https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")

;; Remove "For information about GNU Emacs..." message at startup
(advice-add #'display-startup-echo-area-message :override #'ignore)


;; Suppress the vanilla startup screen completely. Even if disabled with
;; `inhibit-startup-screen', it would still initialize anyway.
(advice-add #'display-startup-screen :override #'ignore)


;;; early-init.el ends here
