;;; Filename: regressionsuite.lisp

;; To use: (regression)

;;; Author: Jasmeet Ajmani

(defparameter *regression-hash* (make-hash-table))

(defparameter units  ;; Unit files (to be tested), including bounds for correlation and mean deviation.
  '(("choice.lisp")
    ("../tutorial/siegler.lisp")
    ("../tutorial/paired.lisp")
    ("zbrodoff.lisp" 0.98 0.2)
    ("../tutorial/sticks.lisp" 0.75 25)
    ("../tutorial/fan.lisp" 0.8 0.07)))

(defparameter *base-location* (directory-namestring *load-truename*))

(defun regression ()
  (loop for files in units
	do
       (load (concatenate 'string *base-location* (first files)))
       (let ((perf
	      (let ((*standard-output* (make-string-output-stream)))
		(unit-test))))
	 (format t "~a: ~a~%" (first files) perf)
	 (setf (gethash (first files) *regression-hash*)
	       (if (and (> (first perf) (or (second files) 0.90))
			(< (second perf) (or (third files) 0.1)))
		   'OK 'FAIL))))
  (print-hash))

(defun print-hash () 
  (loop for value being the hash-values of *regression-hash*
        using (hash-key key)
        do (format t "~&~A -> ~A" key value)))










