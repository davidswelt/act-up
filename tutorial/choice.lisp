;; -*-Mode: Act-up; fill-column: 75; comment-column: 50; -*-
;;; Filename: choice.lisp

;; To use: (unit-test)

;;; Author: Jasmeet Ajmani
;;; Acknowledgements: Dan Bothell

;; (defpackage actup-choice (:use :common-lisp ))
;; (in-package actup-choice)

;; These load commands will find the ACT-UP files
;; relative to the location of the present file:
(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))
(load (concatenate 'string (directory-namestring *load-truename*) "../util/actr-stats.lisp"))



;; Architectural (ACT-R) Parameter(s)

;; [tutorial students - fill in this section:
;;  consider setting parameters relevant to utility learning.]

;; Model parameter:

;; [tutorial students - fill in this section:
;;  modify the parameter.  Perhaps write some code to explore
;;  this parameter systematically!]
(defparameter *positive-reward* 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *choice-data* '(0.66 0.78 0.82 0.84))




;; Experimental environment
(defun toss-coin ()
  "Toss the coin and return heads or tails."
  (if (< (random 1.0) .9) 'heads 'tails))


;; The Model
;;;; Rules that return the choice as symbol heads or tails

(defproc decide-tails ()
  "Make a decision for tails."
  :group choose-coin
  'tails)

(defproc decide-heads ()
  "Make a decision for heads."
  :group choose-coin
  'heads)

;;;; Executing choice functions and assigning rewards
(defproc predict-and-flip-coin ()
  "Choose heads or tails and learn from it."
  (let ((response (choose-coin)))
    (if (eq (toss-coin) response)
	(assign-reward *positive-reward*)
	(assign-reward 0.0))
    response))




;;; Experiment Code

(defun run-block-of-trials (m)
   (let ((count 0))
     (dotimes (i m count)
       (when (eq 'heads (predict-and-flip-coin))  ;; Run the trial model
	 ;; increase counter if it returns `heads'
         (incf count)))))

(defun do-n-blocks-of-trials (n m)
  (let (result)
    (dotimes (i n)
      (push (run-block-of-trials m) result))
    (reverse result)))

(defun print-results-choice (results)
  (format t " Original     Current~%")
  (dotimes (i 4)
    (format t "~8,3F    ~8,3F~%" (nth i *choice-data*) (nth i results)))
  (list (correlation results *choice-data*)
	(mean-deviation results *choice-data*)))

(defun analyze-choice (data)
  (let ((n (length data))
	(result nil))
    (dotimes (i (length (car data)) (reverse result))
      (push (/ (apply #'+ (mapcar #'(lambda (x) (nth i x)) data)) (* n 12)) result))))

(defun collect-data-choice (n)
  (let (data)
    (dotimes (i n)
      (reset-model)
      (push (do-n-blocks-of-trials 4 12) data))
    (print-results-choice (analyze-choice data))))


;;;; Test harness for the experiment

(defun unit-test ()
  (collect-data-choice 100))


;; -*-Mode: ACT-UP; fill-column: 75; comment-column: 50; -*-
