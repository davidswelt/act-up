;; common code

(if (boundp 'emacs-version)
    (progn
      (defmacro defconstant (var val &optional doc)
	`(set (defvar ,var nil ,(or doc "")) ,val))
      (defmacro byte (high low)
	`(cons ,high ,low))
      (defmacro defparameter (var val &optional doc)
	`(set (defvar ,var nil ,(or doc "")) ,val))
      (defun format-cl2el (form)
 	(replace-regexp-in-string "~\\([a-z]\\)" "%\\1"
         (replace-regexp-in-string "~a" "%s"
          (replace-regexp-in-string "~%" "\\n" form))))
      (defun format-nil (form &rest args)
	(apply #'format (format-cl2el form) args))
      (defun format-t (form &rest args)
	(print (apply #'format (format-cl2el form) args))))
  (progn
    (defun format-nil (form &rest args)
      (apply #'format nil form args))
    (defun format-t (form &rest args)
      (apply #'format t form args))))




(when (boundp 'emacs-version)


(require 'cl)

(defmacro export (&rest args) nil)
(defmacro defpackage (&rest args) nil)
(defmacro in-package (&rest args) nil)

(set (defvar *load-truename*)
      (or load-file-name default-directory))


(defun directory-namestring (f)
 (directory-file-name (file-name-directory f))))

