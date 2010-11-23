;;; Filename: regressionsuite.lisp

;; To use: (regression)

;;; Author: Jasmeet Ajmani


(eval-when (:compile-toplevel :load-toplevel :execute) 
  (load (concatenate 'string (directory-namestring (or *load-truename* *compile-file-truename*)) "../load-act-up.lisp")))

(defparameter *regression-hash* (make-hash-table :test 'equal))

(defparameter *units*  ;; Unit files (to be tested), including bounds for correlation and mean deviation.
  '(("choice")
    ("../tutorial/fan" 0.8 0.07)
    ("../tutorial/paired" 0.95 0.2) ;; needs work
    ("../tutorial/siegler")
    ("../tutorial/sticks" 0.75 25)
    ("zbrodoff" 0.98 0.15)))

(defparameter *base-location* (directory-namestring *load-truename*))

(defvar *act-up-avoid-multiple-loading* nil)  ; forward declaration
(defun unit-test () '(1.0 1.0)) ; forward declaration


(defun print-hash () 
  (loop for value being the hash-values of *regression-hash*
        using (hash-key key)
        do (format t "~&~A ~30T-> ~A" key value)))


(defun regression (&optional test-units)
  "Run a regression test over models and print results.
Uses `*units*' to determine tested units."
  (loop
     with *act-up-avoid-multiple-loading* = t
     for files in (or test-units *units*)
	do
       (reset-actup)
       (actup-load (first files) "test")
       (format t "running ~a...~%" (first files))
       (let ((perf
	      (let ((*standard-output* (make-string-output-stream)))
		(unit-test))))
	 (format t "~a: ~a~%" (first files) perf)
	 (setf (gethash (first files) *regression-hash*)
	       (if (and (> (first perf) (or (second files) 0.90))
			(< (second perf) (or (third files) 0.1)))
		   'OK 'FAIL))))
  (print-hash))








