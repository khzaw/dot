;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/basics")

(setq gs-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(require 'defaults)
(require 'layout)
(require 'bindings)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(unless package-archive-contents
  (package-refresh-contents))

(require 'deps)

(provide 'init)












;; (package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))
;; (require 'use-package)
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

;; (require 'org)
;; (org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; (provide 'init) ;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0fe24de6d37ea5a7724c56f0bb01efcbb3fe999a6e461ec1392f3c3b105cc5ac" "e3c64e88fec56f86b49dcdc5a831e96782baf14b09397d4057156b17062a8848" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" "ca70827910547eb99368db50ac94556bbd194b7e8311cfbdbdcad8da65e803be" default))
 '(package-selected-packages
   '(doom-modeline elogt undo-tree projectile yaml-mode ace-window avy rjsx-mode magit js2-mode editorconfig which-key exec-path-from-shell go-mode all-the-icons evil-collection evil-escape evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
