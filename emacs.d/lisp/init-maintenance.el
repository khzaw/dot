;;; -*- lexical-binding: t; -*-

;; Find errors in init.el by bisecting the file
(use-package bug-hunter
  :commands bug-hunter-init-file)

(use-package esup :commands esup)

;; Peak into macros by expanding them inline
(use-package macrostep :commands macrostep-expand)

(provide 'init-maintenance)
