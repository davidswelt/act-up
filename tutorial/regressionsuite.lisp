;;; Filename: regressionsuite.lisp

;; To use: (regression)

;;; Author: Jasmeet Ajmani

(defparameter units  ;; Unit files (to be tested), including bounds for correlation and mean deviation.
  '(("choice.lisp")
    ("siegler.lisp")
    ("paired.lisp")
    ("fan.lisp" 0.8 0.07)))

(defun regression ()
  (loop for i in units
	do
	(progn
	  (load (first i))
	  (print i)
	  (let ((*standard-output* (make-string-output-stream)))  
	    (setq val (unit-test)))    

	  ;; DR: Jasmeet, please fix the following to use a proper
	  ;; `and' expression rather than two nested `if's with redundant
	  ;; inner code (print 'FAIL).
	  
	  (if (> (first val) (or (second i) 0.90))
	      (if (> (second val) (or (third i) 0.1))
		  (print 'FAIL)
		(print 'OK))
	    (print 'FAIL)))))
	

;; DR: Jasmeet, please ensure `val' and `i' are named appropriately, and that
;; `val' is correctly let-bound.

;; DR: Jasmeet, please make sure we get a nice table at the end that contains
;; PASS/FAIL for each unit.  Right now, the output is not very useful, due to
;; massive loading operations for each tutorial unit.









