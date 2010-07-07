;
;
;
;

(require "act-up" "../act-up.lisp")
(use-package :act-up)

(load "../actr-stats")

(setq *egs* 0.7)

(defvar *choice-data* '(0.66 0.78 0.82 0.84))

(defvar *response*)

(defun do-trial-model ()
  (setf *response* (choose-coin))
  (setf feedback (if (< (random 1.0) .9) 'heads 'tails))
  (if (eq feedback *response*)
      (assign-reward 2.0)
    (assign-reward 0.0))
  *response*)

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

(defrule decide-tails ()
  :group choose-coin
  'tails)

(defrule decide-heads ()
  :group choose-coin
  'heads)

