;; -*-Mode: Act-up; fill-column: 75; comment-column: 50; -*-
;;; Filename: paired.lisp

;;; To run use command: (unit-test)

;;; Author: David Reitter and Jasmeet Ajmani
;;; Acknowledgements: Dan Bothell

;; we use a more complex notation to find the ACT-UP file
;; relative to the location of the tutorial file.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (concatenate 'string (directory-namestring (or *load-truename* *compile-file-truename*)) "../load-act-up.lisp"))
  (load (concatenate 'string (directory-namestring (or *load-truename* *compile-file-truename*)) "../util/actr-stats.lisp"))
  (load (concatenate 'string (directory-namestring (or *load-truename* *compile-file-truename*)) "../util/act-up-experiments.lisp")))

;; Architectural (ACT-R) parameters:

(setf *rt* -2)
(setq *lf* 0.4)
(setf *ans* 0.5)
(setf *bll* 0.5)

;; Model parameters:

(defparameter *model-time-parameter* 0.05)

;; The Model

;;;; Define chunk type
(define-chunk-type pair probe answer)
 
(defproc recall-a-number (item)
  :group recall-number
  :initial-utility 1
  (let ((p (retrieve-chunk (list :chunk-type 'pair 
				 :probe item))))
    (pass-time *model-time-parameter*)
    (when p ;; chunk retrieved:
      (learn-chunk p) ;; reinforce the chunk
      (pair-answer p))))

(defproc learn-a-pair (item number)
  :group learn-pair
  :initial-utility 30
  ;; ... now learn / reinforce this item-number pair
  (learn-chunk (make-pair* :probe item 
			   :answer number))
  ;; do not return anything
  nil)

;; Empirical data and experiment materials:

(defvar *pairs* '(("bank" "0") ("card" "1") ("dart" "2") ("face" "3") ("game" "4")
                  ("hand" "5") ("jack" "6") ("king" "7") ("lamb" "8") ("mask" "9")
                  ("neck" "0") ("pipe" "1") ("quip" "2") ("rope" "3") ("sock" "4")
                  ("tent" "5") ("vent" "6") ("wall" "7") ("xray" "8") ("zinc" "9")))

(defvar *paired-latencies* '(0.0 2.158 1.967 1.762 1.680 1.552 1.467 1.402))
(defvar *paired-probability* '(0.000 .526 .667 .798 .887 .924 .958 .954))

(defparameter *response* nil)
(defparameter *response-time* nil)
 
;;;; Test harness for the experiment

(defun unit-test ()
  (collect-data-paired 100))

(defvar *current-number* nil)
(defvar *number-time* nil)
(defun wait-for-number ()
  "Wait until the experimental environment shows the number.  Return number."
  (pass-time (- *number-time* (actup-time)))
  *current-number*)
(defun set-number (number after-seconds)
  (setq *number-time* (+ (actup-time) after-seconds)
	*current-number* number))

(defun do-experiment (size trials)
    (do-experiment-model size trials))

(defun do-experiment-model (size trials)
  (let ((result nil))
    (reset-model) 
    (dotimes (i trials) 
      (let ((score 0.0)
            (time 0.0))
         (dolist (x (subseq *pairs* (- 20 size))) 
	   (let* ((response nil)
		  (duration
		   (stop-actup-time
		     (setq response (recall-number (first x))))))

	     ;; wait 5 seconds before showing the number
	     (pass-time (- 5.0 duration))
	     (if (equal (second x) response)   
		 (progn
		   (assign-reward 20)
		   (incf score 1.0)    
		   (incf time duration))
		 (progn
		   (let ((learn-time 
			  (stop-actup-time
			    (learn-pair (first x) (second x)))))
		     (assign-reward 0)
		     (pass-time (- 5.0 learn-time)))

		   )))) ; show number for 5 seconds
	 (push (list (/ score size) (and (> score 0) (/ time score ))) result)))
    (reverse result)))

(defun collect-data-paired (n)
  (do ((count 1 (1+ count))
       (results (do-experiment 20 8)
                (mapcar #'(lambda (lis1 lis2)
                            (list (+ (first lis1) (first lis2))
                                  (+ (or (second lis1) 0) (or (second lis2) 0))))
                  results (do-experiment 20 8))))
      ((equal count n) 
       (output-data-paired results n))))

(defun output-data-paired (data n)
  (let ((probability (mapcar #'(lambda (x) (/ (first x) n)) data))
        (latency (mapcar #'(lambda (x) (/ (or (second x) 0) n)) data)))
    (let ((lat-perf (print-results-paired latency *paired-latencies* "Latency"))
	  (acc-perf (print-results-paired probability *paired-probability* "Accuracy")))

      ;; Return values (for unit test):
      (list (min (first lat-perf) (first acc-perf)) ;; return lower correlation measure
	    (max (second lat-perf) (second acc-perf)))))) ;; return higher mean dev measure 

(defun print-results-paired (predicted data label)
 (format t "~%~%~A:~%" label)
 (format t "Trial    1       2       3       4       5       6       7       8~%")
 (format t "     ~{~8,3f~}~%" predicted)
 (list (correlation predicted data) (mean-deviation predicted data)))


;; Parameter optimization:

(defun test-procedure-compilation ()
  ;; Alternative parameters with procedure compilation enabled:
  (setf *procedure-compilation* t
	*egs* 0.1
	*lf* 0.3
	*rt* -1.7
	*alpha* 0.4)
  (setq perf (act-up-experiments::optimize-parameters ((*egs* 0.4 0.5 0.1)) (unit-test)))
  (setq len (list-length perf))
  (loop for l from 1 to (- len 2)	   
	do (format t "~a: ~a~%" (first (first (nth (- l 1) perf))) (rest (first (nth (- l 1) perf)))))
  (print (car (last perf)))
  (print (nth (- len 2) perf)))
  ;(act-up-experiments::optimize-parameters ((*alpha* 0 0.8 0.1) (*model-time-parameter* 0 1.0 0.1)) (unit-test)))
;(setq perf (act-up-experiments::optimize-parameters ((*egs* 1.0 5.0 2.0 ) (*ans* 1.0 1.5 0.5) (*lf* 0.0 1.0 0.5) (*mas* 1.0 3.0 1.0) (*bll* 0.0 1.0 0.5))  (unit-test)))
