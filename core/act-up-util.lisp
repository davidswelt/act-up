
(in-package :act-up)

;; VARIOUS


(defun log-safe (val &optional (default 0.0))
  (if (and val (> val 0))
      (log val)
      default))

;; TREE STRUCTURE
;; this implements an unsorted tree

;; (setq tree (act-up::make-tree))
;; (act-up::add-tree-value tree '(one bad day) 55)
;; (act-up::add-tree-value tree '(one bad day in hell) 'ohyeah)
;; (act-up::add-tree-value tree '(one fine day) 'rare)

;; (act-up::get-tree-value tree '(one bad day in hell))
;; (act-up::maptree (lambda (p v) (format t "~a: ~a~%" p v)) tree)

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
  (loop for pe in path
     for elt = (assoc pe tree :test #'equal)
     do
       (unless elt (return nil))
       (setq tree (cdr elt))
     finally ;; found
       (return (cdar tree))))


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



