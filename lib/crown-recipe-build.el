;;; crown-recipe-build.el --- Crown packages building -*- lexical-binding: t; coding:utf-8 -*-

;;; Commentary:

;;

;;; Code:
(require 'cl)
(require 'crown-recipe)




(defvar crown-build-default-files '("*.el" "lisp/*.el"
				    (:exclude
				     ".dir-locals.el" "lisp/.dir-locals.el"
				     "test.el" "tests.el" "*-test.el" "*-tests.el"
				     "lisp/test.el" "lisp/tests.el" "lisp/*-test.el" "lisp/*-tests.el")))


(defvar crown-local-repo (expand-file-name "~/Downloads/Borrar/crown-packages/"))
(defvar crown-packages-path (expand-file-name "repo/" crown-local-repo))

(defvar crown-build-path (expand-file-name "build/" crown-packages-path))
(defvar crown-src-path (expand-file-name "src/" crown-packages-path))

(defvar crown-install-packages (expand-file-name "crown-packages/" ".config/emacs/"))

(defvar crown-recipes-path "~/Projects/crown/crown/recipes/")

(defun crown|git-fetcher (def build-path uri &optional commit)
  (message "GIT CLONE %s" uri)
  (crown|exec-process "git" "clone" uri build-path))
  
(cl-defun crown|recipe-get-source (&key method uri commit)
  (lexical-let ((uri uri)
		(commit commit))
    #'(lambda (def build-path) 
	(crown|git-fetcher def build-path uri commit))))

(cl-defun crown|default-builder (&key files)
  '(lambda (def src-path build-path) 
     (message "Builder func %S %S %S" def src-path build-path)
     (crown|builder def src-path build-path)))

(defun crown|builder (def src-path build-path)
  (crown|walk-dir src-path #'(lambda (src file)
				   (message "(%s)/%s" src file)
				   (crown|copy src file build-path))))

(defun crown|copy-filter (src-path dest-path filter)
  (crown|walk-dir src-path
		  #'(lambda (src file) (crown|copy src file dest-path))
		  #'(lambda (src file) ()))

(defun crown|copy (src-path file dest-path)
  (let ((full-src-path (expand-file-name file src-path))
	(full-dest-path (expand-file-name file dest-path)))
    (if (file-directory-p full-src-path)
	(make-directory full-dest-path t)
      (copy-file full-src-path full-dest-path))))

(defun crown|walk-dir (src fn &optional dir parent filter)
  (let* ((filter (if (not filter) #'(lambda (a b) t) filter))
	 (dir (if (not dir) "." dir))
	(files (directory-files (expand-file-name dir src) nil nil t nil)))
    (mapc #'(lambda (path)
	      (if (not (string= "." (substring path 0 1)))
		  (let ((full-sub-path (if parent
					   (concat (concat parent "/") path)
					 path))) 
		    (if (funcall filter src full-sub-path)
			(if (file-directory-p (expand-file-name full-sub-path src))
			    (progn
			      (funcall fn src full-sub-path)
			      (crown|walk-dir src fn full-sub-path full-sub-path))
			  (funcall fn src full-sub-path)))))) files)))

(cl-defun crown|default-installer ()
  '(lambda (def build-path install-path)
     (message "Installer func %S %S %S" def build-path install-path)))

(defun crown|search-recipe (name &optional local-path) 
  (let* ((name (symbol-name name))
         (path (or local-path crown-recipes-path)))
    (if (file-exists-p path)
	(progn 
          (load (expand-file-name (concat name ".el") path))
          (crown|get-recipe name))
      (user-error "No recipe found in %s for package %s" path name))))

(defun crown|build-recipe (def src-path build-path install-path)
  (let* ((name (symbol-name (crown|recipe-name def)))
	 (src-path (expand-file-name  name src-path))
	 (build-path (expand-file-name  name build-path))
	 (install-path (expand-file-name  name install-path))
	 (sourcef (crown|recipe-source def))
	 (buildf (crown|recipe-build def))
	 (installf (crown|recipe-install def)))
    (if (file-exists-p build-path)
	(progn
	  (@trace (format "Directory (%s) exists, deleting" build-path))
	  (delete-directory build-path t nil)))
    (@trace (format "Sorucing (%s)" name))
    (funcall sourcef def src-path)
    (@trace (format "Building (%s)" name))
    (funcall buildf def src-path build-path)
    (@trace (format "Installing (%s)" name))
    (funcall installf def build-path install-path)))

(setq current-recipe-def 
      (crown|search-recipe 'evil crown-recipes-path))

(message "Current %S" current-recipe-def)

(crown|build-recipe 
  current-recipe-def crown-src-path crown-build-path crown-install-packages)

(provide 'crown-recipe-build)
;;; crown-recipe-build.el ends here
