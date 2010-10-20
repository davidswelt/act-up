;;; Filename: sticks.lisp

;;; To run use command: (collect-data 10000)

;;; Author: Jasmeet Ajmani
;;; Acknowledgements: Dan Bothell

;; we use a more complex notation to find the ACT-UP file
;; relative to the location of the tutorial file.
(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))
(load (concatenate 'string (directory-namestring *load-truename*) "../util/actr-stats.lisp"))

(defvar *strategy*)
(setq *strategy* nil)
(setq *egs* 3.0)

;; model parameter
(defparameter *model-time-parameter* 1.5)

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

;; MODEL

;;;; Procedures that form a group of strategies to solve the problem

(defproc decide-over (current A B C G) :initial-utility 13
  :group (choose-strategy-over)
  (setq *strategy* 'over)
  (if (= current 0)
      (setq current B))
  (loop while (> current G) do 
	(cond
	 ((= (- (- current G) C) 0) (setq current (- current C)))
	 ((= (- (- current G) A) 0) (setq current (- current A)))
	 ((> (- (- current G) A)  (- (- current G) C)) (setq current (- current C)))
	 ((< (- (- current G) A)  (- (- current G) C)) (setq current (- current A))))))

(defproc decide-under (current A B C G) :initial-utility 13
  :group (choose-strategy-under)
  (setq *strategy* 'under)
  (if (= current 0)
      (setq current C))
  (loop while (< current G) do
	(cond
	 ((= (- (- G current) C) 0) (setq current (+ current C)))
	 ((= (- (- G current) A) 0) (setq current (+ current A)))
	 ((> (- (abs(- current G)) A)  (- (abs(- current G)) C)) (setq current (+ current C)))
	 ((< (- (abs(- current G)) A)  (- (abs(- current G)) C)) (setq current (+ current A))))))

(defproc force-over (current A B C G) :initial-utility 10
  :group (choose-strategy-over choose-strategy-under choose-strategy-force)
  (setq *strategy* 'over)
  (if (= current 0)
      (setq current B))
  (loop while (> current G) do
	(cond
	 ((= (- (- current G) C) 0) (setq current (- current C)))
	 ((= (- (- current G) A) 0) (setq current (- current A)))
	 ((> (- (- current G) A)  (- (- current G) C)) (setq current (- current C)))
	 ((< (- (- current G) A)  (- (- current G) C)) (setq current (- current A))))))

(defproc force-under (current A B C G) :initial-utility 10
  :group (choose-strategy-over choose-strategy-under choose-strategy-force)
  (setq *strategy* 'under)
  (if (= current 0)
      (setq current C))
  (loop while (< current G) do
	(cond
	 ((= (- (- G current) C) 0) (setq current (+ current C)))
	 ((= (- (- G current) A) 0) (setq current (+ current A)))
	 ((> (- (abs(- current G)) A)  (- (abs(- current G)) C)) (setq current (+ current C)))
	 ((< (- (abs(- current G)) A)  (- (abs(- current G)) C)) (setq current (+ current A))))))


(defproc bst (A B C G) ;;;; A=smallest stick, B=longest stick, C=medium stick and G=target stick
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
      (choose-strategy-force current A B C G)))
    (pass-time *model-time-parameter*)
    (cond
      ((= current G) (assign-reward 20))
      ((> current G) (assign-reward 0)))
    ))



;;;; Test harness for the experiment

(defun do-set ()
   (let ((result nil))
     (dolist (stim *bst-stimuli*)
       (bst (nth 0 stim) (nth 1 stim) (nth 2 stim) (nth 3 stim))
       (push *strategy* result))
     (reverse result)))

(defun collect-data (n)
   (let ((result (make-list (length *bst-stimuli*) :initial-element 0)))
     (dotimes (i n result)
       (setf result (mapcar #'+ result (mapcar #'(lambda (x) (if (equal x 'over) 1 0)) (do-set)))))

     (setf result (mapcar #'(lambda (x) (* 100.0 (/ x n))) result))

     (format t "~%Trial ")

     (dotimes (i (length result))
       (format t "~8s" (1+ i)))

     (format t "~%  ~{~8,2f~}~%~%" result)

     (when (= (length result) (length *bst-exp-data*))
       (list (correlation result *bst-exp-data*)
	     (mean-deviation result *bst-exp-data*)))))


;; Parameter optimization

(defun optimize-parameters ()
  (with-open-file (output "fan-results.txt" :direction :output
			  :if-exists :supersede)
    (format output "initial.delay~ttime.factor~tcorrelation~tdeviation~%")
    (let ((minimum 0.1)
	  (maximum 4)
	  (best-cor 0.0)
	  (best-cor-par nil)
	  (best-meandev 1000.0)
	  (best-meandev-par nil)
	  ;; do not print extra output:
	  (*actr-stats-show-results* nil))
    (loop with *actr-stats-show-results* = nil
       for *model-time-parameter* from minimum to maximum by 0.1
	    for performance = (collect-data 1000)
	    do
	      (let* (
		    (cor (first performance))
		    (meandev (second performance)))
		(if (> cor best-cor)
		    (setq best-cor cor
			  best-cor-par (list  *model-time-parameter*)))
		(if (< meandev best-meandev)
		    (setq best-meandev meandev
			  best-meandev-par (list  *model-time-parameter*)))

		(format output "~,4F ~t~,4F~t~,4F~%" 
		  *model-time-parameter*
			cor
			meandev)))

    (format t "Best correlation: ~,4F at parameters ~a~%"
	    best-cor best-cor-par)
    (format t "Best mean deviation: ~,4F at parameters ~a~%"
	    best-meandev best-meandev-par))))