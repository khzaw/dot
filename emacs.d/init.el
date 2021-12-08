
;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/basics")

(setq gs-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(setq insert-directory-program "/usr/local/bin/gls")
(setq dired-listing-switches "-aBhl --group-directories-first")

(require 'defaults)
(require 'layout)
(require 'bindings)


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package use-package-ensure-system-package)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
  '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(require 'deps)
(require 'git)
(require 'python)
(require 'go)
(require 'web)

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
 '(ansi-color-names-vector
   ["#1c1e26" "#e95678" "#09f7a0" "#fab795" "#21bfc2" "#6c6f93" "#59e3e3" "#c7c9cb"])
 '(custom-safe-themes
   '("0fe24de6d37ea5a7724c56f0bb01efcbb3fe999a6e461ec1392f3c3b105cc5ac" "e3c64e88fec56f86b49dcdc5a831e96782baf14b09397d4057156b17062a8848" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" "ca70827910547eb99368db50ac94556bbd194b7e8311cfbdbdcad8da65e803be" default))
 '(fci-rule-color "#f9cec3")
 '(jdee-db-active-breakpoint-face-colors (cons "#16161c" "#e95678"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#16161c" "#09f7a0"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#16161c" "#6a6a6a"))
 '(objed-cursor-color "#e95678")
 '(package-selected-packages
   '(quelpa pyenv move-text shackle restart-emacs all-the-icons-ivy-rich paredit aggressive-indent poetry auto-package-update tide flycheck plantuml-mode plantumi-mode python-mode ivy-posframe ag add-node-modules-path prettier-js smartparens-config treemacs-icons-dired yasnippet dap-mode bufler posframe git-timemachine amx all-the-icons-dired lsp-ivy company-box terraform-mode lsp-ui restclient typescript-mode lsp-mode fzf keyfreq counsel swiper ivy-rich ivy ace-jump-mode company doom-modeline elogt undo-tree projectile yaml-mode ace-window avy rjsx-mode magit js2-mode editorconfig which-key exec-path-from-shell go-mode all-the-icons evil-collection evil-escape evil use-package))
 '(pdf-view-midnight-colors (cons "#c7c9cb" "#1c1e26"))
 '(rustic-ansi-faces
   ["#1c1e26" "#e95678" "#09f7a0" "#fab795" "#21bfc2" "#6c6f93" "#59e3e3" "#c7c9cb"])
 '(vc-annotate-background "#1c1e26")
 '(vc-annotate-color-map
   (list
    (cons 20 "#09f7a0")
    (cons 40 "#59e19c")
    (cons 60 "#a9cc98")
    (cons 80 "#fab795")
    (cons 100 "#f6ab8f")
    (cons 120 "#f39f89")
    (cons 140 "#f09383")
    (cons 160 "#c48788")
    (cons 180 "#987a8d")
    (cons 200 "#6c6f93")
    (cons 220 "#95668a")
    (cons 240 "#bf5e81")
    (cons 260 "#e95678")
    (cons 280 "#c95b74")
    (cons 300 "#a96071")
    (cons 320 "#89656d")
    (cons 340 "#f9cec3")
    (cons 360 "#f9cec3")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
