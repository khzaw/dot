(setq default-frame-alist
  (append (list
	    '(font . "MonoLisa:size=14")
	    '(min-height . 1)  '(height     . 45)
	    '(min-width  . 1)  '(width      . 90)
      '(vertical-scroll-bars . nil)
   ;; '(internal-border-width . 24)
      '(tool-bar-lines . 0)
      '(menu-bar-lines . 0))))
(setq-default line-spacing 2)

(defun transparency (value)
  "Set the transparency of the frame window to VALUE 0=transparent/100=opaque."
  (interactive "nTransparency Value (0 - 100) :")
  (set-frame-parameter (selected-frame) 'alpha value))
(transparency 97)

;; Enable transparent title bar on macOS
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . light)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))


(setq inhibit-startup-screen t
  inhibit-startup-message t
  inhibit-startup-echo-area-message t
  initial-scratch-message nil)
(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
;; (global-hl-line-mode 1)
(setq x-underline-at-descent-line t)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Hide org markup for README
(setq org-hide-emphasis-markers t)

;; When split is automatic, always split windows vertically
(setq split-height-threshold 0)
(setq split-width-threshold nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

(provide 'layout)
