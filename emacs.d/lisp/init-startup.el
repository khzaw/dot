;;; init-startup.el --- Startup scheduling -*- lexical-binding: t; -*-

(defun khz/run-on-idle (function delay &rest arguments)
  "Run FUNCTION with ARGUMENTS after startup is idle for DELAY seconds."
  (let (schedule)
    (setq schedule
          (lambda ()
            (remove-hook 'emacs-startup-hook schedule)
            (apply #'run-with-idle-timer delay nil function arguments)))
    (if after-init-time
        (funcall schedule)
      (add-hook 'emacs-startup-hook schedule))))

(defun khz/preload-on-idle (feature delay)
  "Load FEATURE after startup has been idle for DELAY seconds.
Commands and hooks can still load the library before the timer runs."
  (khz/run-on-idle (lambda () (unless (featurep feature) (require feature)))
                   delay))

(provide 'init-startup)
;;; init-startup.el ends here
