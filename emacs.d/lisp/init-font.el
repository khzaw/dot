;; -*- lexical-binding: t; -*-

(setq text-scale-mode-step 1.1)

;;; https://github.com/mickeynp/ligature.el/wiki
(use-package ligature
  :straight (:type git :host github :repo "mickeynp/ligature.el")
  :hook ((prog-mode text-mode) . ligature-mode)
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures
   'prog-mode
   '(; Group A
     ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
     ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
     ; Group B
     "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
     "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
     "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
     "|=" "//=" "/="
     ; Group C
     "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
     "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
     "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
     ; Group D
     "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
     ; Group E
     "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
     "{!--" "//" "///" "!!"
     ; Group F
     "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
     ; Group G
     "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
     "--" "---")))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun all-faces-at-point (pos)
  "Show all faces at POS (or point if called interactively)."
  (interactive "d")
  (let* ((faces '())
         ;; Get faces from overlays
         (overlays (overlays-at pos)))
    (dolist (ov overlays)
      (let ((face (overlay-get ov 'face)))
        (when face
          (if (listp face)
              (setq faces (append face faces))
            (push face faces)))))
    ;; Get face from text properties
    (let ((text-face (get-text-property pos 'face)))
      (when text-face
        (if (listp text-face)
            (setq faces (append text-face faces))
          (push text-face faces))))
    ;; Deduplicate
    (setq faces (delete-dups (delq nil faces)))
    (if faces
        (message "Faces at %d: %s" pos faces)
      (message "No faces at %d" pos))))

(global-set-key (kbd "C-h M-f") #'all-faces-at-point)

(defun khz/graphic-frame-live-p ()
  "Return non-nil when Emacs has at least one graphical frame."
  (catch 'graphic
    (dolist (frame (frame-list))
      (when (display-graphic-p frame)
        (throw 'graphic t)))))

(defun khz/fontaine-current-preset ()
  "Return the current Fontaine preset, falling back to `regular'."
  (or (and (boundp 'fontaine-current-preset)
           fontaine-current-preset)
      (and (fboundp 'fontaine-restore-latest-preset)
           (fontaine-restore-latest-preset))
      'regular))

(defun khz/fontaine-apply-current-preset (&rest _)
  "Reapply the active Fontaine preset after face-resetting events."
  (when (and (khz/graphic-frame-live-p)
             (fboundp 'fontaine-set-preset))
    (fontaine-set-preset (khz/fontaine-current-preset))))

(defun khz/reload-font-config (&optional preset)
  "Reload `init-font.el' and apply PRESET, or the current preset when nil."
  (interactive)
  (unless (khz/graphic-frame-live-p)
    (user-error "Font reloading is only useful in a graphical frame"))
  (let ((preset (or preset (khz/fontaine-current-preset))))
    (load-file (locate-user-emacs-file "lisp/init-font.el"))
    (require 'fontaine)
    (fontaine-set-preset preset)
    (message "Reloaded Fontaine preset: %s" preset)))

(use-package fontaine
  :config
  (let ((line-spacing (pcase system-type
                        ('gnu/linux 0.05)
                        ('darwin 3)
                        (_ nil))))
    (setq fontaine-presets
          `((regular
             :default-height 130
             :default-family "Berkeley Mono"
             :fixed-pitch-family "Berkeley Mono"
             :variable-pitch-family "IBM Plex Sans"
             :variable-pitch-height 1.0)
            (writing
             :default-height 160
             :default-family "Berkeley Mono"
             :fixed-pitch-family "Berkeley Mono"
             ;; Charter: Matthew Carter's serif, optimized for screen reading.
             ;; Pairs well with monospace at a slight size bump.
             :variable-pitch-family "Charter"
             :variable-pitch-weight normal
             :variable-pitch-height 1.2)
            (reading
             :default-height 150
             :default-family "Berkeley Mono"
             :fixed-pitch-family "Berkeley Mono"
             :variable-pitch-family "IBM Plex Sans"
             :variable-pitch-height 1.0)
            (presentation
             :default-height 200
             :default-family "Berkeley Mono"
             :fixed-pitch-family "Berkeley Mono"
             ;; Avenir Next: clean geometric sans, excellent at large sizes.
             :variable-pitch-family "Avenir Next"
             :variable-pitch-weight medium
             :variable-pitch-height 1.0)
            (t
             :line-spacing ,line-spacing))))
  (khz/fontaine-apply-current-preset)
  (add-hook 'after-make-frame-functions #'khz/fontaine-apply-current-preset)
  (add-hook 'enable-theme-functions #'khz/fontaine-apply-current-preset)
  (add-hook 'after-load-theme-hook #'khz/fontaine-apply-current-preset)
  (fontaine-mode 1))

(provide 'init-font)
