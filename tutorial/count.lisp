;;; Filename: count.lisp

;;; Author: Jasmeet Ajmani

(require "act-up" "../act-up.lisp")
(use-package :act-up)

;; ACT-R parameters
(setq *lf* .05)
(setq *rt* -1)

;;;; Defining chunk type

(define-chunk-type count-order first second)

;;;; Committing chunks to memory

(learn-chunk (make-count-order :name 'one-two :first 1 :second 2))
(learn-chunk (make-count-order :name 'two-three :first 2 :second 3))
(learn-chunk (make-count-order :name 'three-four :first 3 :second 4))
(learn-chunk (make-count-order :name 'four-five :first 4 :second 5))
(learn-chunk (make-count-order :name 'five-six :first 5 :second 6))

;;;; Defining procedural rule

(defrule count-model (arg1 arg2) 
  "Count from ARG1 to ARG1.
ARG1 is the starting point and ARG2 is the ending point.
Each increment is 1 unit."
  (speak arg1)
  (if (not (eq arg1 arg2))
      (let ((p (retrieve-chunk (list :chunk-type 'count-order 
				     :first arg1))))
	(if p
	    (count-model (count-order-second p) arg2)))
      ;; else return end point
      arg2))

(defrule speak (number)
  "Say number."
  (print number)
  (pass-time 0.3))