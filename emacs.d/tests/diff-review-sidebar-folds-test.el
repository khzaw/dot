;;; diff-review-sidebar-folds-test.el --- Sidebar fold tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'diff-review-sidebar-folds)

(defmacro diff-review-sidebar-folds-test--fixture (&rest body)
  "Run BODY with a synthetic review and its real sidebar renderer."
  (declare (indent 0) (debug t))
  `(let ((review (generate-new-buffer " *fold-test-review*"))
         (sidebar (generate-new-buffer " *fold-test-sidebar*")))
     (unwind-protect
         (progn
           (with-current-buffer review
             ;; The sidebar only needs these review fields, not a Git diff.
             (setq major-mode 'diff-review-mode
                   diff-review--viewed (make-hash-table :test #'equal)
                   diff-review--comments []
                   diff-review--index 0
                   diff-review--files
                   [(:path "src/api/first.el" :status "M" :hash "one" :added 1 :removed 0)
                    (:path "src/api/second.el" :status "M" :hash "two" :added 1 :removed 0)
                    (:path "src/model/third.el" :status "M" :hash "three" :added 1 :removed 0)
                    (:path "tests/fourth.el" :status "M" :hash "four" :added 1 :removed 0)]))
           (with-current-buffer sidebar
             (diff-review-sidebar-mode)
             (setq diff-review--review review)
             (diff-review-sidebar--render)
             ,@body))
       (kill-buffer sidebar)
       (kill-buffer review))))

(defun diff-review-sidebar-folds-test--directory (name)
  "Find directory section NAME in the current sidebar."
  (let (found)
    (cl-labels ((find-directory (section)
                  (when (and (eq (oref section type) 'diff-review-dir)
                             (equal (oref section value) name))
                    (setq found section))
                  (mapc #'find-directory (oref section children))))
      (find-directory magit-root-section))
    (or found (ert-fail (format "No directory section %s" name)))))

(ert-deftest diff-review-sidebar-folds-preserves-ancestors-when-switching-files ()
  (diff-review-sidebar-folds-test--fixture
    (magit-section-hide (diff-review-sidebar-folds-test--directory "api"))
    (magit-section-hide (diff-review-sidebar-folds-test--directory "src"))
    (with-current-buffer review (setq diff-review--index 1))
    (diff-review-sidebar--render)
    (should (oref (diff-review-sidebar-folds-test--directory "src") hidden))
    (should (oref (diff-review-sidebar-folds-test--directory "api") hidden))
    (should (eq (magit-current-section)
                (diff-review-sidebar-folds-test--directory "src")))))

(ert-deftest diff-review-sidebar-folds-preserves-other-folders-and-repeat-redraws ()
  (diff-review-sidebar-folds-test--fixture
    (magit-section-hide (diff-review-sidebar-folds-test--directory "tests"))
    (with-current-buffer review (setq diff-review--index 3))
    (dotimes (_ 3)
      (diff-review-sidebar--render)
      (should (oref (diff-review-sidebar-folds-test--directory "tests") hidden)))
    (with-current-buffer review (setq diff-review--index 2))
    (diff-review-sidebar--render)
    (should (oref (diff-review-sidebar-folds-test--directory "tests") hidden))
    (should-not (oref (diff-review-sidebar-folds-test--directory "src") hidden))))

(ert-deftest diff-review-sidebar-folds-honors-manual-expansion ()
  (diff-review-sidebar-folds-test--fixture
    (magit-section-hide (diff-review-sidebar-folds-test--directory "api"))
    (with-current-buffer review (setq diff-review--index 1))
    (diff-review-sidebar--render)
    (magit-section-show (diff-review-sidebar-folds-test--directory "api"))
    (with-current-buffer review (setq diff-review--index 0))
    (diff-review-sidebar--render)
    (should-not (oref (diff-review-sidebar-folds-test--directory "api") hidden))))

(ert-deftest diff-review-sidebar-folds-leaves-normal-section-reveal-alone ()
  (diff-review-sidebar-folds-test--fixture
    (let* ((directory (diff-review-sidebar-folds-test--directory "api"))
           (file (car (oref directory children))))
      (magit-section-hide directory)
      (magit-section-reveal file)
      (should-not (oref directory hidden)))))

;;; diff-review-sidebar-folds-test.el ends here
