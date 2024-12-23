;;; crown-recipe-build.el --- Crown packages building -*- lexical-binding: t; coding:utf-8 -*-

;;; Commentary:

;;

;;; Code:
(require 'cl)
(require 'crown-recipe)

(defvar crown-build-path "/home/tristanstille/Borrar/evil")
(defvar crown-packages-path "")
(defvar crown-recipes-path "/home/tristanstille/Projects/crown/recipies/")

(defun crown|git-fetcher (def build-path uri &optional commit)
  (message "GIT CLONE %s" uri)
  (crown|exec-process "git" "clone" uri build-path))
  
(cl-defun crown|recipe-get-source (&key method uri commit)
          (lexical-let ((uri uri)
                        (commit commit))
                       #'(lambda (def build-path) 
                           (crown|git-fetcher def build-path uri commit))))

(cl-defun crown|default-builder (&key files)
          '(lambda (def build-path packages-path) 
             (message "Builder func %S %S %S" def build-path packages-path)))

(defun crown|search-recipe (name &optional local-path) 
  (let* ((name (symbol-name name))
         (path (or local-path crown-recipes-path)))
    (if (file-exists-p path)
      (progn 
        (load (expand-file-name (concat name ".el") path))
        (crown|get-recipe name))
      (user-error "No recipe found in %s for package %s" path name))))

(defun crown|build-recipe (def build-path packages-path)
  (let* ((name (crown|recipe-name def))
         (build-path (expand-file-name  (symbol-name name) build-path))
         (sourcef (crown|recipe-source def))
         (buildf (crown|recipe-build def)))
      (funcall sourcef def build-path)
      (funcall buildf def build-path packages-path)))

(setq current-recipe-def 
      (crown|search-recipe 'evil crown-recipes-path))

(message "Current %S" current-recipe-def)

(crown|build-recipe 
  current-recipe-def crown-build-path crown-packages-path)

(provide 'crown-recipe-build)
;;; crown-recipe-build.el ends here
