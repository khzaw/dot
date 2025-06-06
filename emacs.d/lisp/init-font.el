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

(use-package fontaine)





(provide 'init-font)
