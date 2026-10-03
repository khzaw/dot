;;; init-optional.el --- Entry points for optional integrations -*- lexical-binding: t; -*-

;; Register the packages and commands now, and load their configuration before
;; the first command runs.  This also retains discovery through M-x.
(use-package org-present
  :commands org-present
  :config (require 'init-presentation))

(use-package dslide
  :straight (dslide :type git :host github
                    :repo "positron-solutions/dslide")
  :commands (dslide-mode dslide-deck-start dslide-deck-develop dslide-deck-present)
  :config (require 'init-presentation))

(use-package epresent
  :after evil
  :straight (:type git :host github :repo "eschulte/epresent"
             :build (:not compile))
  :commands epresent-run
  :config (require 'init-presentation))

(use-package erc
  :straight (:type built-in)
  :commands (erc erc-tls)
  :config (require 'init-erc))

(provide 'init-optional)
;;; init-optional.el ends here
