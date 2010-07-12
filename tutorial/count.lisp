;;; Filename: count.lisp

;;; Author: Jasmeet Ajmani

(require "act-up" "../act-up.lisp")
(use-package :act-up)

(setq *lf* .05)
(setq *rt* -1)

;;;; Defining chunk type

(define-chunk-type count-order first second)

;;;; Committing chunks to memory

(learn-chunk (make-count-order :name 'a :first 1 :second 2))
(learn-chunk (make-count-order :name 'b :first 2 :second 3))
(learn-chunk (make-count-order :name 'c :first 3 :second 4))
(learn-chunk (make-count-order :name 'd :first 4 :second 5))
(learn-chunk (make-count-order :name 'e :first 5 :second 6))

;;;; Defining procedural rule

(defrule count-model (arg1 arg2) 
  "arg1 is the starting point and arg2 is the ending point"
  "each increment is 1 unit"
  (print arg1)
  (if (not (eq arg1 arg2))
      (let ((p (retrieve-chunk (list :chunk-type 'count-order 
				     :first arg1))))
	(progn
	  (setq arg1 (count-order-second p))
	  (count-model arg1 arg2)))))