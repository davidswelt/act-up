;; -*-Mode: Act-up; fill-column: 75; comment-column: 50; -*-
;;; Filename: grouped.lisp

;; To use: (recall)

;;; Author: Jasmeet Ajmani

(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))
(load (concatenate 'string (directory-namestring *load-truename*) "../util/actr-stats.lisp"))

(setq *rt* -5
      *ans* .15
      *mp* 1.0
      *declarative-finst-span* 20
      *declarative-num-finsts* 15)

;;;; Defining chunk types

(define-chunk-type item number parent position)

;;;; Committing chunks to memory

(defun init-model ()
  (reset-model)
  (loop for x in '(1 2 3)
	for y in '(first second third)
	collect (learn-chunk (make-item :number x :parent 'group1 :position y)))
  (loop for x in '(4 5 6)
	for y in '(first second third)
	collect (learn-chunk (make-item :number x :parent 'group2 :position y)))
  (loop for x in '(7 8 9)
	for y in '(first second third)
	collect (learn-chunk (make-item :number x :parent 'group3 :position y))))

;;;; Setting similarities between chunks
(set-similarities-fct '((first second -0.5)
			(second third -0.5)
			(first third -1)
			(group1 group2 -1)
			(group2 group3 -1)
			(group1 group3 -1)))

;;;; Test harness for the experiment

;;;; Defining Procedural Rules

(defproc recall () 
  (init-model)
  (loop for g in '(group1 group2 group3)
	collect (loop for p in '(first second third)
		      collect (item-number (retrieve-chunk (list :chunk-type 'item)
							  nil
							  (list :position p
								:parent g)
							  nil
							  :recently-retrieved nil)))))
							       
