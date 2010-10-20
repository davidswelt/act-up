;;; Filename: paired.lisp

;;; To run use command: (unit-test)

;;; Author: Jasmeet Ajmani
;;; Acknowledgements: Dan Bothell


;; we use a more complex notation to find the ACT-UP file
;; relative to the location of the tutorial file.
(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))
(load (concatenate 'string (directory-namestring *load-truename*) "../util/actr-stats.lisp"))


(setf *rt* -2)
(setf *ans* 0.5)
(setf *bll* 0.4)
(setf *lf* 0.3)

(defvar *pairs* '(("bank" "0") ("card" "1") ("dart" "2") ("face" "3") ("game" "4")
                  ("hand" "5") ("jack" "6") ("king" "7") ("lamb" "8") ("mask" "9")
                  ("neck" "0") ("pipe" "1") ("quip" "2") ("rope" "3") ("sock" "4")
                  ("tent" "5") ("vent" "6") ("wall" "7") ("xray" "8") ("zinc" "9")))

(defvar *paired-latencies* '(0.0 2.158 1.967 1.762 1.680 1.552 1.467 1.402))
(defvar *paired-probability* '(0.000 .526 .667 .798 .887 .924 .958 .954))
(defparameter *correlation-paired* nil)
(defparameter *meandev-paired* nil)
(defparameter *response* nil)
(defparameter *response-time* nil)
 
;;;; Define chunk type
(define-chunk-type pair probe answer)
 
;;;; Test harness for the experiment

(defun unit-test ()
  (collect-data-paired 100))

(defun do-experiment (size trials)
    (do-experiment-model size trials))

(defun do-experiment-model (size trials)
  (let ((result nil))

    (reset-model) 

    (dotimes (i trials) 
      (let ((score 0.0)
            (time 0.0)
            (start-time))
         (dolist (x (subseq *pairs* (- 20 size))) 

          (setf *response* nil)                   
          (setf *response-time* nil)
          (setf start-time (actup-time))

	  (setf *response* (paired x))
	  (setf *response-time* (actup-time))
	  (pass-time 3.5)

          (when (equal (second x) *response*)      
            (incf score 1.0)    
            (incf time (- *response-time* start-time))))
	 (pass-time 5)
	  
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
       (output-data-paired results n)))
  (list *correlation-paired* *meandev-paired*))

(defun output-data-paired (data n)
   (let ((probability (mapcar #'(lambda (x) (/ (first x) n)) data))
        (latency (mapcar #'(lambda (x) (/ (or (second x) 0) n)) data)))
    (print-results-paired latency *paired-latencies* "Latency")
     (print-results-paired probability *paired-probability* "Accuracy")))

(defun print-results-paired (predicted data label)
 (format t "~%~%~A:~%" label)
  (setq *correlation-paired* (correlation predicted data))
  (setq *meandev-paired* (mean-deviation predicted data))
  (format t "Trial    1       2       3       4       5       6       7       8~%")
  (format t "     ~{~8,3f~}~%" predicted))

;;;; Defining procedural rule

(defproc paired (arg)
  (let ((p (retrieve-chunk (list :chunk-type 'pair 
				 :probe (first arg)))))
    (if (not p)
	(progn
	  (setq p (make-pair :probe (first arg) 
			     :answer (second arg)))
	  (learn-chunk p))
      (progn
	(learn-chunk p)
	(pair-answer p)))))