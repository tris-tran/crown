;;; process.el --- Crown process run -*- lexical-binding: t; coding:utf-8 -*-

;;; Commentary:

;;

;;; Code:
(@debug "crown-lib process")

(defun crown|exec-process (comnd &rest args)
  (with-temp-buffer
    (cons (let ((process
                  (make-process :name "crown-exec"
                                :buffer (current-buffer)
                                :command (cons comnd (remq nil args))
                                :connection-type 'pipe))
                done-exec)
          (set-process-filter
            process (lambda (_process output)
                      (princ output (current-buffer))
                      (message "%s" output)))
          (set-process-sentinel
            process (lambda (process _event)
                      (when (memq (process-status process) '(exit stop))
                        (setq done-exec t))))
          (while (not done-exec)
                 (sit-for 0.1)
                 (process-exit-status process)))
          (string-trim (buffer-string)))))

(provide 'crown-lib '(process))
;;; process.el ends here
