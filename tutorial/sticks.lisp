;;; Filename: sticks.lisp

;;; Author: Jasmeet Ajmani
;;; Acknowledgements: Dan Bothell

;;; To run use command: collect-data 10000

(require "act-up" "../act-up.lisp")
(use-package :act-up)

(load "actr-stats")

(defvar *strategy*)
(setq *strategy* nil)
(setq *egs* 3.0)

(defvar *bst-exp-data* '(20.0 67.0 20.0 47.0 87.0 20.0 80.0 93.0
                         83.0 13.0 29.0 27.0 80.0 73.0 53.0))

(defparameter *bst-stimuli* '((15  250  55  125)(10  155  22  101)
			      (14  200  37  112)(22  200  32  114)
			      (10  243  37  159)(22  175  40  73)
			      (15  250  49  137)(10  179  32  105)
			      (20  213  42  104)(14  237  51  116)
			      (12  149  30  72)
			      (14  237  51  121)(22  200  32  114)
			      (14  200  37  112)(15  250  55  125)))

;;;; Test harness for the experiment

(defun do-set ()
   (let ((result nil))
     (dolist (stim *bst-stimuli*)
       (bst (nth 0 stim) (nth 1 stim) (nth 2 stim) (nth 3 stim))
       (push *strategy* result)
       (pass-time 1.5))
     (reverse result)))

(defun collect-data (n)
   (let ((result (make-list (length *bst-stimuli*) :initial-element 0)))
     (dotimes (i n result)
       (setf result (mapcar #'+ result (mapcar #'(lambda (x) (if (equal x 'over) 1 0)) (do-set)))))

     (setf result (mapcar #'(lambda (x) (* 100.0 (/ x n))) result))

     (when (= (length result) (length *bst-exp-data*))
       (correlation result *bst-exp-data*)
       (mean-deviation result *bst-exp-data*))

     (format t "~%Trial ")

     (dotimes (i (length result))
       (format t "~8s" (1+ i)))

     (format t "~%  ~{~8,2f~}~%~%" result)))

(defun bst (A B C G) ;;;; A=smallest stick, B=longest stick, C=medium stick and G=target stick
  (let ((under (- G C))
	(over (- B G))
	(current 0)) 
    (cond
     ((< over (- under 25))
      (choose-strategy-over current A B C G))
     ((not (eq *strategy* 'over)) 
      (choose-strategy-force current A B C G))
     ((< under (- over 25))
      (choose-strategy-under current A B C G))
     ((not (eq *strategy* 'under))
      (choose-strategy-force current A B C G)))))

;;;; Defrules that form a group of strategies to solve the problem

(defrule decide-over (current A B C G) :initial-utility 13
  :group (choose-strategy-over)
  (setq *strategy* 'over)
  (if (= current 0)
      (setq current B))
  (loop while (> current G) do 
	(cond
	 ((= (- (- current G) C) 0) (setq current (- current C)))
	 ((= (- (- current G) A) 0) (setq current (- current A)))
	 ((> (- (- current G) A)  (- (- current G) C)) (setq current (- current C)))
	 ((< (- (- current G) A)  (- (- current G) C)) (setq current (- current A)))))
  (cond
   ((= current G) (assign-reward 20))
   ((< current G) (assign-reward 0))))

(defrule decide-under (current A B C G) :initial-utility 13
  :group (choose-strategy-under)
  (setq *strategy* 'under)
  (if (= current 0)
      (setq current C))
  (loop while (< current G) do
	(cond
	 ((= (- (- G current) C) 0) (setq current (+ current C)))
	 ((= (- (- G current) A) 0) (setq current (+ current A)))
	 ((> (- (abs(- current G)) A)  (- (abs(- current G)) C)) (setq current (+ current C)))
	 ((< (- (abs(- current G)) A)  (- (abs(- current G)) C)) (setq current (+ current A)))))
  (cond
   ((= current G) (assign-reward 20))
   ((> current G) (assign-reward 0))))

(defrule force-over (current A B C G) :initial-utility 10
  :group (choose-strategy-over choose-strategy-under choose-strategy-force)
  (setq *strategy* 'over)
  (if (= current 0)
      (setq current B))
  (loop while (> current G) do
	(cond
	 ((= (- (- current G) C) 0) (setq current (- current C)))
	 ((= (- (- current G) A) 0) (setq current (- current A)))
	 ((> (- (- current G) A)  (- (- current G) C)) (setq current (- current C)))
	 ((< (- (- current G) A)  (- (- current G) C)) (setq current (- current A)))))
  (cond
   ((= current G) (assign-reward 20))
   ((< current G) (assign-reward 0))))

(defrule force-under (current A B C G) :initial-utility 10
  :group (choose-strategy-over choose-strategy-under choose-strategy-force)
  (setq *strategy* 'under)
  (if (= current 0)
      (setq current C))
  (loop while (< current G) do
	(cond
	 ((= (- (- G current) C) 0) (setq current (+ current C)))
	 ((= (- (- G current) A) 0) (setq current (+ current A)))
	 ((> (- (abs(- current G)) A)  (- (abs(- current G)) C)) (setq current (+ current C)))
	 ((< (- (abs(- current G)) A)  (- (abs(- current G)) C)) (setq current (+ current A)))))
  (cond
   ((= current G) (assign-reward 20))
   ((> current G) (assign-reward 0))))



