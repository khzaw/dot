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
    native-comp-deferred-compilation t))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
  gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 gc-cons-percentage 0.1)))

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
;;; early-init.el ends here
