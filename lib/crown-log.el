; # -*- lexical-binding: t -*-

(defvar loglevels '((trace . 4)
                    (debug . 3) 
                    (info . 2)
                    (warn . 1)
                    (error . 0)
                    (log . -1)))

(defvar crown-loglevel 4)

(defvar log-ansi-escape
  '(;; fx
    (reset      . 0)
    (bold       . 1)
    (dark       . 2)
    (italic     . 3)
    (underscore . 4)
    (blink      . 5)
    (rapid      . 6)
    (contrary   . 7)
    (concealed  . 8)
    (strike     . 9)
    ;; fg
    (black      . 30)
    (red        . 31)
    (green      . 32)
    (yellow     . 33)
    (blue       . 34)
    (magenta    . 35)
    (cyan       . 36)
    (white      . 37)
    ;; bg
    (on-black   . 40)
    (on-red     . 41)
    (on-green   . 42)
    (on-yellow  . 43)
    (on-blue    . 44)
    (on-magenta . 45)
    (on-cyan    . 46)
    (on-white   . 47)))

(defvar crown-print-styles '((log . (lambda () 
                                        (setq log-ansi-default (log|ansi-escape '(white)))
                                        (log|apply-ansi 'white "[PRINT DEBUG]: ")))
                             (trace . (lambda () 
                                        (setq log-ansi-default (log|ansi-escape '(cyan)))
                                        (log|apply-ansi 'cyan "[TRACE]: ")))
                             (debug . (lambda () 
                                        (setq log-ansi-default (log|ansi-escape '(blue)))
                                        (log|apply-ansi 'blue "[DEBUG]: ")))
                             (info . (lambda () 
                                        (setq log-ansi-default (log|ansi-escape '(white)))
                                        (log|apply-ansi 'white "[INFO]: ")))
                             (warn . (lambda () 
                                        (setq log-ansi-default (log|ansi-escape '(yellow)))
                                        (log|apply-ansi 'yellow "[WARN]: ")))
                             (error . (lambda () 
                                        (setq log-ansi-default (log|ansi-escape '(red)))
                                        (log|apply-ansi 'red "[ERROR]: ")))))

(defun log|prompt (level)
  (funcall (cdr (assq level crown-print-styles))))

(defun log|get-level (level) 
  (cdr (assq level loglevels)))

(defun log|format (msg)
  (message "MENSAJE [%s]" msg)
  (concat log-default-style
          (format-spec msg log-ansi-format-spec)
          "\e[0m"))

(defun log|ansi-escape (codes) 
  (mapconcat (lambda (code) 
               (format "\e[0%dm" (cdr (assq code log-ansi-escape))))
             codes
             ""))

(defun log|apply-ansi (style msg)
  (let* ((code (cdr (assq style log-ansi-escape)))
         (msg msg))
    (format "\e[0%dm%s%s%s" code msg log-ansi-reset log-ansi-default)))

(defvar log-ansi-default
  (log|ansi-escape '(green)))
(defvar log-ansi-reset
  (log|ansi-escape '(reset)))

(defun log|apply-format (forms)
  (let ((forms (if (listp forms)
                 (if (= (length forms) 1)
                   (car forms)
                   forms)
                 forms)))
    (cond ((null forms) nil)
          ((listp forms) 
           (cond ((not (symbolp (car forms)))
                  (apply #'format 
                         (car forms)
                         (mapcar #'(lambda (x) (log|apply-format x))
                                 (cdr forms))))
                 ((assq (car forms) log-ansi-escape)
                  (log|apply-ansi 
                    (car forms) 
                    (log|apply-format (cdr forms))))
                 (t (format "%s" forms))))
          ((format "%s" forms)))))

(defun @log (msg &rest objs) (apply #'log|log 'log msg objs))
(defun @trace (msg &rest objs) (apply #'log|log 'trace msg objs))
(defun @debug (msg &rest objs) (apply #'log|log 'debug msg objs))
(defun @info (msg &rest objs) (apply #'log|log 'info msg objs))
(defun @warn (msg &rest objs) (apply #'log|log 'warn msg objs))
(defun @error (msg &rest objs) (apply #'log|log 'error msg objs))

(defun log|log (level msg &rest objs)
  (let ((leveln (log|get-level level))
        (prompt (log|prompt level)))
    (if (<= leveln crown-loglevel)
      (apply #'message (concat prompt msg log-ansi-reset) (mapcar #'(lambda (x) (log|apply-format x)) objs)))))

;(defvar pepe "pepe")
;(@log "[%s]" '("pepe |%s| |%s|" (red ("holl %s" (bold (blue "pepe %s" (yellow "ana"))))) (yellow "yellow")))
;(@debug "pepe |%s| |%s|" 
;        '(red ("holl %s" (bold (blue "pepe %s" (underscore (yellow "ana"))))))
;        '(yellow "yellow %s" (cyan pepe)))
;(@trace "solo-string |%s|" "string")
;(@debug "solo-string |%s|" "string")
;(@info "solo-string |%s|" "string")
;(@warn "solo-string |%s|" "string")
;(@error "solo-string |%s|" '(one two))

(provide 'crown-log)
