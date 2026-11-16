;;; diff-review-sidebar-folds.el --- Keep manual sidebar folds -*- lexical-binding: t; -*-

;;; Commentary:
;; diff-review reveals the selected file's parents when the current file
;; changes.  Restore manually collapsed directories after that redraw.

;;; Code:

(require 'cl-lib)
(require 'diff-review)

(defun diff-review-sidebar-folds--collapsed ()
  "Return the identities of collapsed directories in this sidebar."
  (let (collapsed)
    (cl-labels ((collect (section)
                  (when (and (eq (oref section type) 'diff-review-dir)
                             (oref section hidden))
                    (push (magit-section-ident section) collapsed))
                  (mapc #'collect (oref section children))))
      (when magit-root-section (collect magit-root-section)))
    collapsed))

(defun diff-review-sidebar-folds--visible-point ()
  "Keep sidebar point on the outermost collapsed parent of its target."
  (let ((section (magit-current-section)) hidden)
    (while section
      (when (oref section hidden) (setq hidden section))
      (setq section (oref section parent)))
    (when hidden (goto-char (oref hidden start)))
    (dolist (window (get-buffer-window-list nil nil t))
      (set-window-point window (point)))))

(defun diff-review-sidebar-folds--render (render &rest args)
  "Call sidebar RENDER with ARGS, restoring manually collapsed folders."
  (let ((collapsed (diff-review-sidebar-folds--collapsed)))
    (prog1 (apply render args)
      (dolist (identity collapsed)
        (when-let* ((section (magit-get-section identity)))
          (magit-section-hide section)))
      (diff-review-sidebar-folds--visible-point))))

(advice-add 'diff-review-sidebar--render :around #'diff-review-sidebar-folds--render)

(provide 'diff-review-sidebar-folds)
;;; diff-review-sidebar-folds.el ends here
