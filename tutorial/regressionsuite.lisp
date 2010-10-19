;;; Filename: regressionsuite.lisp

;; To use: (regression)

;;; Author: Jasmeet Ajmani

(defparameter source-files '("choice.lisp" "siegler.lisp" "paired.lisp" "fan.lisp" ))

(defun regression ()
  (loop for i in source-files
	do
	(progn
	  (load i)
	  (print i)
	  (let ((*standard-output* (make-string-output-stream)))  
	    (setq val (unit-test)))     
	  (if (> (first val) 0.90)
	      (if (> (second val) 0.1)
		  (print ' FAIL)
		(print 'OK))
	    (print 'FAIL)))))
	











