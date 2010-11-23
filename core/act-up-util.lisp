(declaim (optimize (speed 0) (space 0) (debug 03)))

(in-package :act-up)


;; VARIOUS

;; anaphoric lisp macros
;; taken from "On Lisp" by Paul Graham.
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (sym (gensym)))
      `(let ((,sym ,(car cl1)))
	 (if ,sym
	     (let ((it ,sym)) ,@(cdr cl1))
	   (acond ,@(cdr clauses)))))))


(defun log-safe (val &optional (default 0.0))
  (if (and val (> val 0))
      (log val)
      default))

;; TREE STRUCTURE
;; this implements an unsorted tree

(defun tree-test ()
  (let ((tree (act-up::make-tree)))
    (act-up::add-tree-value tree '(one bad day) 55)
    (act-up::add-tree-value tree '(one bad day in hell) 'ohyeah)

    (act-up::add-tree-value tree '(one bad day in heaven) 'boring)
    (act-up::add-tree-value tree '(one bad day without hell) 'ohyeah-w)
    (act-up::add-tree-value tree '(one fine day) 'rare)
    (act-up::add-tree-value tree '(one * day) 'standard)
    (act-up::add-tree-value tree '(one * day * hell) 'standard-22)
    (act-up::add-tree-value tree '(one * day in hell) 'standard-24)
    (act-up::get-tree-value tree '(one bad day in hell))
    (act-up::maptree (lambda (p v) (format t "~a: ~a~%" p v)) tree)
    (format t "one superb day:  ~a~%" (equal '(STANDARD) (get-tree-value-with-wildcards tree '(one superb day))))
    (format t "one superb day on earth:  ~a~%" (eq nil (get-tree-value-with-wildcards tree '(one superb day on earth))))
    
    (format t "(with wildcard)  one fine day:  ~a~%" (equal '(RARE STANDARD) (get-tree-value-with-wildcards tree '(one fine day))))
    (format t "(without wildcard)  one fine day:  ~a~%" (eq 'rare (get-tree-value tree '(one fine day))))

    (format t "one bad day in hell:  ~a~%" (equal '(OHYEAH
						    STANDARD-24
						    STANDARD-22)
						  (get-tree-value-with-wildcards tree '(one  bad day in hell))))))

;;;;;;;;
;; prefix trees

(defun make-tree (&optional value)
  (list (cons :tree-structure value)))

;; recursive
;; (defun add-tree-value (tree path value)
;;   (let ((elt (assoc (car path) tree)))
;;     (unless elt
;;       (setq elt (cons (car path) (make-tree)))
;;       (setf (cdr (last tree)) (cons elt nil)))
;;     (if (cdr path)
;; 	(add-tree-value (cdr elt) (cdr path) value)
;;     ;; end of path
;; 	(setf (cdr elt) (make-tree value)))))
 

;; non-recursive implementation:
(defun add-tree-value (tree path value &optional append)
  "with APPEND non-nil, leaf is a list.  add value to that list rather than replacing it."
  (loop with prev = nil
     for pe in path
     for elt = (assoc pe tree :test #'equal)
     do
       (unless elt
	 (setq elt (cons pe (make-tree)))
	 (setf (cdr (last tree)) (cons elt nil)))
       (setq prev elt)
       (setq tree (cdr elt))
     finally
     ;; end of path
       (if append
	    (setf (cdr (last (car (cdr prev))))
		  (list value))
	    (setf (cdr prev) 
		  (append 
		   (make-tree value)
		   (cdr (cdr prev)))))))

(defun get-tree-leaf-create (tree path)
  "Gets tree leaf at PATH.  Creates path in tree if necessary.
The tree leaf is a list that can be changed freely (setf (cdr ..) ...)."
  (loop with prev = nil
     for pe in path
     for elt = (assoc pe tree :test #'equal)
     do
       (unless elt
	 (setq elt (cons pe (make-tree)))
	 (setf (cdr (last tree)) (cons elt nil)))
       (setq prev elt)
       (setq tree (cdr elt))
     finally
     ;; end of path
       (return (car (cdr prev)))))


(defun get-tree-value (tree path)
  "Get value in TREE, path PATH."
  (loop for pe in path
     for elt = (assoc pe tree :test #'equal)
     do
       (unless elt (return nil))
       (setq tree (cdr elt))
     finally ;; found
       (return (cdar tree))))

(defun get-tree-values-with-wildcards (tree path)
  "Get values in TREE, path PATH.
Tree may contain `wildcard' symbols, which match
any choice in path.  Where a wildcard `*' and a symbol
in the tree both match, this function will
recurse into each branch."
  (let ((agenda (list (cons path tree))))
    (loop while agenda append   ;; collect all solutions
	 (let* ((item (pop agenda))
		(path (car item))
		(tree (cdr item)))
	   (loop
	      for pe-rest on path
	      for elt = (assoc (car pe-rest) tree :test #'equal)
	      for elt-w = (assoc '* tree :test #'eq)
	      do
		(if elt-w
		    (push (cons (cdr pe-rest) (cdr elt-w)) agenda))
		;; follow specific entry immediately,
		;; follow wildcard entry laster
		(unless elt (return nil))
		(setq tree (cdr elt))
	      finally ;; found at end of path
		(return (list (cdar tree))))))))

(defun maptree (function tree &optional path)
  "Call FUNCTION for each leaf in TREE.
FUNCTION must be a function taking two arguments: the path (a list)
and the value of the leaf."
  (unless (and (consp tree)
	       (consp (car tree))
	       (eq :tree-structure (car (car tree))))
    (error "mapte: not a tree."))
  (if (cdr (car tree))
      (funcall function path (cdr (car tree))))
  (loop for (k . v) in (cdr tree) do
       (let  ((path (append path (list k))))
	 (maptree function v path))))

;;;




;; Dictionary class
;; hybrid between association list and hash table
;; This collection uses an assoc list internally for small sets
;; and a hash table for large ones.
;; This is optimized for reading speed.

(defparameter *dict-threshold* 10)
#+:sbcl (setq *dict-threshold* 27)
#+:allegro (setq *dict-threshold* 9)
#+:openmcl (setq *dict-threshold* 6)
#+:lispworks (setq *dict-threshold* 12)

(defun make-dictionary (&optional (size 1))
  (if (> size *dict-threshold*)
      (make-hash-table :size size)
      (list '(assoc-list-len . 0))))

(defun dict-get (item dict)
  (if (hash-table-p dict)
      (gethash item dict)
      (cdr (assoc item dict))))

(defun dict-set (dict item value)
  (declare (type (or hash-table list) dict))  ;
    (if (hash-table-p dict)
	  (setf (gethash item dict) value)
	;; check length
	(let ((found (assoc item dict)))
	  (if found
	      (setf (cdr found) value)
	      ;; we're adding.  check length
	      (progn
		(setf (cdr (last dict)) (list (cons item value)))
		(if (and (eq (caar dict) 'assoc-list-len)
			 (>= (cdar dict) *dict-threshold*))
		    (let ((new-d (make-dictionary (* 2 (cdar dict)))))
		      (loop for (k . v) in (cdr dict) do
			   (setf (gethash k new-d) v))
		      (setq dict new-d))
		    ;; increase list length
		    (incf (cdar dict)))))))
    dict)

;; (defun dict-test ()
;;   (let ((d (make-dictionary 3)))
;;     (loop for i from 1 to 30 do
;; 	(setq d (dict-set d i (- 1000 i))))
;;     d))
