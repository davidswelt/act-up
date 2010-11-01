;; -*-Mode: Act-up; fill-column: 75; comment-column: 50; -*-

;;; Filename: sticks.lisp

;;; To run use command: (collect-data 10000)

;;; Authors: Jasmeet Ajmani and David Reitter
;;; Acknowledgements: Dan Bothell

;; These load commands will find the ACT-UP files
;; relative to the location of the present file:
(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))
(load (concatenate 'string (directory-namestring *load-truename*) "../util/actr-stats.lisp"))

;; Architectural parameters
(setq *egs* 3.25)

;; Model parameters
(defparameter *model-time-parameter* 0.2)
(defparameter *model-reward* 20)


;;; Empirical data

(defvar *bst-exp-data* '(20.0 67.0 20.0 47.0 87.0 20.0 80.0 93.0
                         83.0 13.0 29.0 27.0 80.0 73.0 53.0))

(defparameter *bst-stimuli* '((15  250  55  125) (10  155  22  101)
			      (14  200  37  112) (22  200  32  114)
			      (10  243  37  159) (22  175  40  73)
			      (15  250  49  137) (10  179  32  105)
			      (20  213  42  104) (14  237  51  116)
			      (12  149  30  72)
			      (14  237  51  121) (22  200  32  114)
			      (14  200  37  112) (15  250  55  125)))

;; MODEL

;;;; Procedures that form a group of strategies to solve the problem

(defvar *current* 0)
(defvar *short* 0)
(defvar *long* 0)
(defvar *medium* 0)
(defvar *goal* 0)

(defun init-model (short-stick-len long-stick-len 
		   medium-stick-len goal-stick-len)
  (setq *current* 0
	*short* short-stick-len
	*long* long-stick-len
	*medium* medium-stick-len
	*goal* goal-stick-len))

(defproc strategy-over ()
  (setq *current* *long*)
  (loop while (> *current* *goal*) do 
       (pass-time *model-time-parameter*)
       (setq *current* (cond
			 ((>= (- *current* *medium*) *goal*) 
			  (- *current* *medium*))
			 ((>= (- *current* *short*) *goal*) 
			  (- *current* *short*)) 
			 (t (return)))))
  'over)

(defproc strategy-under ()
  (setq *current* *medium*)
  (loop while (< *current* *goal*) do
       (pass-time *model-time-parameter*)
       (setq *current* (cond
		       ((<= (+ *current* *medium*) *goal*)
			(+ *current* *medium*))
		       ((<= (+ *current* *short*) *goal*) 
			(+ *current* *short*)) 
		       (t (return)))))
  'under)


(defproc decide-over  () :initial-utility 13
  :group (prefer-over)
  (strategy-over))

(defproc decide-under  () :initial-utility 13
  :group (prefer-under)
  (strategy-under))

(defproc force-over  () :initial-utility 10
  :group  (prefer-over prefer-under either-strategy)
  (strategy-over))

(defproc force-under  () :initial-utility 10
  :group  (prefer-over prefer-under either-strategy)
  (strategy-under))


(defproc bst (strategy short long medium goal)
  (init-model short long medium goal)

  (let ((c-under (- goal medium))
	(b-over (- long goal)))

    (prog1  ;; return this (strategy)
	(cond
	  ((< b-over (- c-under 25)) 
	   ;; if long is much better than medium
	   (prefer-over))
	  ((not (eq strategy 'over))
	   (either-strategy))
	  ((< c-under (- b-over 25))  
	   ;; if medium is much better than long
	   (prefer-under))
	  (t (either-strategy)))
      (assign-reward
       (if (= *current* goal) *model-reward* 0)))))



;;;; Test harness for the experiment

(defun do-set ()
  (reset-model)
  (let ((result nil) (current-strategy nil))
    (dolist (stim *bst-stimuli*)
      (push
       (setq current-strategy
	     (bst current-strategy (nth 0 stim) (nth 1 stim) (nth 2 stim) (nth 3 stim)))
       result))
    (reverse result)))

(defun collect-data (n &optional silently)
   (let ((result (make-list (length *bst-stimuli*) :initial-element 0)))
     (dotimes (i n result)
       (setf result (mapcar #'+ result (mapcar #'(lambda (x) (if (equal x 'over) 1 0)) (do-set)))))

     (setf result (mapcar #'(lambda (x) (* 100.0 (/ x n))) result))

     (unless silently
       (format t "~%Trial ")
       (dotimes (i (length result))
	 (format t "~8s" (1+ i)))
       
       (format t "~%  ~{~8,2f~}~%~%" result))
     (when (= (length result) (length *bst-exp-data*))
       (list (correlation result *bst-exp-data*)
	     (mean-deviation result *bst-exp-data*)))))

;; Unit testing

(defun unit-test ()
  (collect-data 20000 'silent))


;; Parameter optimization

(defun optimize-parameters ()
  (with-open-file (output "sticks-results.txt" :direction :output
			  :if-exists :supersede)
    (format output "initial.delay~ttime.factor~tcorrelation~tdeviation~%")
    (let ((minimum 0.2)
	  (maximum 2.5)
	  (best-cor 0.0)
	  (best-cor-par nil)
	  (best-meandev 1000.0)
	  (best-meandev-par nil)
	  ;; do not print extra output:
	  (*actr-stats-show-results* nil))
    (loop 
       for *model-reward* from 10 to 30 by 5 do
    (loop with *actr-stats-show-results* = nil
       for *model-time-parameter* from minimum to maximum by 0.1
	    for performance = (collect-data 20000 'silent)
	    do
	 (let* ((cor (first performance))
		(meandev (second performance)))
	   (if (> cor best-cor)
	       (setq best-cor cor
		     best-cor-par (list *model-reward* *model-time-parameter* *model-time-parameter-2*)))
	   (if (< meandev best-meandev)
	       (setq best-meandev meandev
		     best-meandev-par (list *model-reward* *model-time-parameter* *model-time-parameter-2*)))
		(format output "~,4F ~t~,4F~t~,4F~%" 
			*model-time-parameter*
			cor
			meandev))))
    (format t "Best correlation: ~,4F at parameters ~a~%"
	    best-cor best-cor-par)
    (format t "Best mean deviation: ~,4F at parameters ~a~%"
	    best-meandev best-meandev-par))))