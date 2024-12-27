;;; process.el --- Crown process run -*- lexical-binding: t; coding:utf-8 -*-

;;; Commentary:

;;

;;; Code:
(@debug "crown-lib process")

(setq stdout #'external-debugging-output)

(defun crown|call-process (cmnd &rest args)
  (with-temp-buffer
    (cons (or (apply #'call-process cmnd nil t nil (remq nil args))
              -1)
          (string-trim (buffer-string)))))

(defun crown|exec-process (comnd &rest args)
  (let ((process
          (make-process :name "crown-exec"
                        :buffer (current-buffer)
                        :command (cons comnd (remq nil args))
                        :connection-type 'pipe))
        done-exec)
    (set-process-filter
      process (lambda (_process output)
                (princ (format "%s" output) stdout)))
    (set-process-sentinel
      process (lambda (process _event)
                (when (memq (process-status process) '(exit stop))
                  (setq done-exec t))))
    (while (not done-exec)
           (sit-for 0.1)
           (process-exit-status process))))

(provide 'crown-lib '(process))
;;; process.el ends here
