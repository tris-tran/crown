;;; crown-recipie.el --- Crown packages definitions -*- lexical-binding: t; coding:utf-8 -*-

;;; Commentary:

;;

;;; Code:

;(cl-deftype func 

(cl-defstruct (crown|recipe (:type list) :named 
                (:constructor crown|new-recipe))
              (name :read-only)
              (version :read-only)
              (source :read-only)
              (build :read-only)
              (package :read-only)
              (install :read-only)
              (dependencies :read-only)
              (doc :read-only))

(defvar crown--recipes (make-hash-table :test 'equal))

(defun crown|def-recipe (&rest def) 
  (puthash (symbol-name (car (cdr def)))
            (apply #'crown|new-recipe def) crown--recipes))

(defun crown|get-recipe (name) 
  (let ((name (if (symbolp name) 
                (symbol-name name)
                name)))
    (gethash name crown--recipes)))

(provide 'crown-recipe)
;;; crown-recipe.el ends here
