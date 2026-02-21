;; -*- lexical-binding: t; -*-

(setq text-scale-mode-step 1.1)

;;; https://github.com/mickeynp/ligature.el/wiki
(use-package ligature
  :straight (:type git :host github :repo "mickeynp/ligature.el")
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
     "--" "---"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

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

(use-package fontaine
  :config
  (setq fontaine-presets
        '((regular
           :default-height 140
           :default-family "Berkeley Mono"
           :fixed-pitch-family "Berkeley Mono"
           :variable-pitch-family "Iosevka Etoile"
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
           ;; Iosevka Etoile: quasi-proportional serif from the Iosevka family.
           ;; Same vertical metrics as Iosevka so code and prose align cleanly.
           :variable-pitch-family "Iosevka Etoile"
           :variable-pitch-height 1.0)
          (presentation
           :default-height 200
           :default-family "Berkeley Mono"
           :fixed-pitch-family "Berkeley Mono"
           ;; Avenir Next: clean geometric sans, excellent at large sizes.
           :variable-pitch-family "Avenir Next"
           :variable-pitch-weight medium
           :variable-pitch-height 1.0)))
  (fontaine-set-preset 'regular)
  ;; Persist last-used preset across sessions
  (fontaine-mode 1))

(provide 'init-font)
