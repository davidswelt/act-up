;; -*-Mode: Act-up; fill-column: 75; comment-column: 50; -*-
;;; Filename: choice.lisp

;; To use: (unit-test)

;;; Author: Jasmeet Ajmani
;;; Acknowledgements: Dan Bothell

;; we use a more complex notation to find the ACT-UP file
;; relative to the location of the tutorial file.

;; (defpackage actup-choice (:use :common-lisp ))
;; (in-package actup-choice)

(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))
(load (concatenate 'string (directory-namestring *load-truename*) "../util/actr-stats.lisp"))

;; ACT-R Parameters
(setq *egs* 0.7)

(defparameter *correlation-choice* nil)
(defparameter *meandev-choice* nil)

(defvar *choice-data* '(0.66 0.78 0.82 0.84))

(defun do-block-of-m-trials (m)
   (let ((count 0))
     (dotimes (i m count)
       (when (eq 'heads (do-trial-model))
         (incf count)))))

(defun do-n-blocks-of-m-trials (n m)
     (let (result)
     (dotimes (i n (reverse result))
       (push (do-block-of-m-trials m) result))))

(defun collect-data-choice (n)
  (let (data)
    (dotimes (i n)
      (reset-model)
      (push (do-n-blocks-of-m-trials 4 12) data))
    (print-results-choice (analyze-choice data)))
  (list *correlation-choice* *meandev-choice*))

(defun analyze-choice (data)
  (let ((n (length data))
	(result nil))
    (dotimes (i (length (car data)) (reverse result))
      (push (/ (apply #'+ (mapcar #'(lambda (x) (nth i x)) data)) (* n 12)) result))))

(defun print-results-choice (results)
  (setq *correlation-choice* (correlation results *choice-data*))
  (setq *meandev-choice* (mean-deviation results *choice-data*))
  (format t " Original     Current~%")
  (dotimes (i 4)
    (format t "~8,3F    ~8,3F~%" (nth i *choice-data*) (nth i results))))

;;;; Test harness for the experiment

(defun unit-test ()
  (collect-data-choice 100))

;; Experimental environment
(defun toss-coin ()
  (if (< (random 1.0) .9) 'heads 'tails))

;; The Model
;;;; Rules that return the choice as symbol heads or tails

(defproc decide-tails ()
  :group choose-coin
  'tails)

(defproc decide-heads ()
  :group choose-coin
  'heads)

;;;; Executing choice functions and assigning rewards
(defproc do-trial-model ()
  "Choose heads or tails and learn from it."
  (let ((response (choose-coin)))
    (if (eq (toss-coin) response)
	(assign-reward 2.0)
	(assign-reward 0.0))
    response))


;; -*-Mode: ACT-UP; fill-column: 75; comment-column: 50; -*-
