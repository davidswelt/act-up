;;; Filename: choice.lisp

;; To use: (do-trial-model)

;;; Author: Jasmeet Ajmani
;;; Acknowledgements: Dan Bothell

(require "act-up" "../act-up.lisp")
(use-package :act-up)

(load "../actr-stats.lisp")


;; ACT-R Parameters
(setq *egs* 0.7)

(defvar *choice-data* '(0.66 0.78 0.82 0.84))

;;;; Test harness for the experiment

(defun do-block-of-m-trials (m)
   (let ((count 0))
     (dotimes (i m count)
       (when (eq 'heads (do-trial-model))
         (incf count)))))

(defun do-n-blocks-of-m-trials (n m)
     (let (result)
     (dotimes (i n (reverse result))
       (push (do-block-of-m-trials m) result))))

(defun collect-data (n)
   (let (data)
     (dotimes (i n)
       (reset-model)
       (push (do-n-blocks-of-m-trials 4 12) data))
     (print-results (analyze data))))

(defun analyze (data)
   (let ((n (length data))
         (result nil))
     (dotimes (i (length (car data)) (reverse result))
       (push (/ (apply #'+ (mapcar #'(lambda (x) (nth i x)) data)) (* n 12)) result))))

(defun print-results (results)
   (correlation results *choice-data*)
   (mean-deviation results *choice-data*)
   (format t " Original     Current~%")
   (dotimes (i 4)
     (format t "~8,3F    ~8,3F~%" (nth i *choice-data*) (nth i results))))

;; Experimental environment
(defun toss-coin ()
  (if (< (random 1.0) .9) 'heads 'tails))

;; The Model
;;;; Rules that return the choice as symbol heads or tails

(defrule decide-tails ()
  :group choose-coin
  'tails)

(defrule decide-heads ()
  :group choose-coin
  'heads)

;;;; Executing choice functions and assigning rewards
(defrule do-trial-model ()
  "Choose heads or tails and learn from it."
  (let ((response (choose-coin)))
    (if (eq (toss-coin) response)
	(assign-reward 2.0)
	(assign-reward 0.0))
    response))

