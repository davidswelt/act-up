;; -*-Mode: Act-up; fill-column: 75; comment-column: 50; -*-
;;; Filename: grouped.lisp

;; To use: (run-recall)

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
  (loop for x from 1 to 9
     for g in '(group1 group1 group1 group2 group2 group2 group3 group3 group3)
     for p in '(first second third first second third first second third)
     do 
       (learn-chunk (make-item :number x :parent g :position p)))
  ;; Setting similarities between chunks
  (set-similarities-fct '((first second -0.5)
			  (second third -0.5)
			  (first third -1)
			  (group1 group2 -1)
			  (group2 group3 -1)
			  (group1 group3 -1))))

;;;; Test harness for the experiment

(defun run-recall ()
  (init-model)
  (recall))

;;;; Defining Procedures

(defproc recall ()
  (loop for g in '(group1 group2 group3)
	collect (loop for p in '(first second third)
		      collect (item-number (retrieve-chunk (list :chunk-type 'item)
							   :soft-spec (list :position p
									    :parent g)
							   :recently-retrieved nil)))))

;;  Initialize
(init-model)