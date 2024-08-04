;; Native compilation stuff
(when (featurep 'native-compile)
  (defvar package-native-compile)
  (defvar native-comp-always-compile)
  (defvar native-comp-async-report-warnings-errors)

  (setq native-comp-speed 2
        ;; Enable ahead-of-time compilation when installing a package
        package-native-compile t
        ;; Silence compiler warnings
        native-comp-async-report-warnings-errors nil
        ;; Compile loaded packages asynchronously
        native-comp-deferred-compilation t

        ;; Prefer loading newer compiled files
        load-prefer-newer t

        warning-suppress-log-types '((comp))))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
  gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)))


;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Set default coding system
(set-language-environment "UTF-8")

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(right-divider-width . 20))
  (add-to-list var '(internal-border-width . 20)))

;;;; Clean View
;; UI - Disable visual cruft

;; Resizing the Emacs frame can be an expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setopt frame-inhibit-implied-resize t
        ;; HACK: Don't show size info (or anything else) in frame title
        frame-title-format "\n"
        ;; Disable start-up screen
        inhibit-startup-screen t
        inhibit-startup-message t
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


;;; early-init.el ends here
