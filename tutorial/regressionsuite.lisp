;;; Filename: regressionsuite.lisp

;; To use: (regression)

;;; Author: Jasmeet Ajmani

(defparameter *regression-hash* (make-hash-table))

(defparameter units  ;; Unit files (to be tested), including bounds for correlation and mean deviation.
  '(("choice.lisp")
    ("siegler.lisp")
    ("paired.lisp")
    ("zbrodoff.lisp")
    ("fan.lisp" 0.8 0.07)))

(defun regression ()
  (loop for files in units
	do
	(progn
	  (load (first files))
	  (let ((*standard-output* (make-string-output-stream)))  
	    (progn
	      (setq val (unit-test)) 
	      (setf (gethash (first files)  
			     (if (and (> (first val) (or (second files) 0.90))
				      (< (second val) (or (third files) 0.1)))
				 'OK 'FAIL)))))))
  (print-hash))

(defun print-hash () 
  (loop for value being the hash-values of *regression-hash*
        using (hash-key key)
        do (format t "~&~A -> ~A" key value)))










