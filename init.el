
(message "init.el")

(load-theme 'tango)

(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)

(require 'evil)
(evil-mode 1)
