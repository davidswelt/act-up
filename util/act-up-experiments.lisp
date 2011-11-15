
(defpackage :act-up-experiments
  (:use :common-lisp))

(in-package :act-up-experiments)


(declaim (optimize (speed 0) (space 0) (debug 3)))
 
(defparameter *talking* t)
(defun say (form &rest args)
  (if *talking*
       (format t "~a ~a: ~a~%" (meta-process-name *current-actUP-meta-process*) (actup-time) (apply 'format nil form args))))

(defun mean (&rest list)
  (if (cdr list)
      (/ (sum list) (length list))
      (/ (sum (car list)) (length (car list)))))

(defun sum (&rest list)
      (loop for x in (if (cdr list) list (car list))
	 sum x))
      
(defun stdev (list &optional mean)
  "sample standard deviation of elements of list."
  (let ((mean (or mean (mean list))))
    (sqrt (/ (loop for x in list sum (expt (- x mean) 2)) (1- (length list))))))
 
(defun cor (list-1 list-2)
  "Pearson's correlation between elements of list-1 and list-2
The elements of LIST-1 and LIST-2 may be lists themselves, in which
case COR returns a list of correlation coefficients."
  (if (listp (car list-1))
      (loop for a in list-1 for b in list-2 collect (cor a b))
      (let ((n (length list-1))
	    (mean-a (mean list-1))
	    (mean-b (mean list-2)))
	(unless (= n (length list-2))
	  (error "cor: lists must be of same length."))
	(/
	 (loop for a in list-1 for b in list-2 sum
	      (* (- a mean-a) (- b mean-b)))
	 (* (1- n) (stdev list-1 mean-a) (stdev list-2 mean-b))))))

(defun rmse (list-1 list-2)
  (let ((n (length list-1)))
    (unless (= n (length list-2))
      (error "rmse: lists must be of same length."))
    (sqrt (/ (loop for a in list-1 for b in list-2 sum
		  (expt (- a b) 2)) n))))

(export '(mean sum stdev cor rmse))

(defun repeat-seq (n sequence)
  (if (> n 0)
      (append sequence (repeat-seq (1- n) sequence))
      nil))

(export '(say mean sum repeat-seq))

(defun structure-alist (object)
  (let ((spec (cdr (second (read-from-string (concatenate 'string "( \\" (prin1-to-string object) " )"))))))
    (loop for argval on spec by #'cddr collect
	 (cons (first argval) (second argval)))))

(defun structure-value-count (object &rest values)
  (let ((spec (cdr (second (read-from-string (concatenate 'string "( \\" (prin1-to-string object) " )"))))))
    (loop for argval on spec by #'cddr sum
	 (if (member (second argval) values) 1 0))))




(defparameter *act-up-random-state* (make-random-state t))

(defun choice (sequence &optional (random-state *act-up-random-state*))
  (and sequence
       (elt sequence (random (length sequence) random-state))))

(defun pick-some (num sequence &optional (random-state *act-up-random-state*))
  (if (and (> num 0) sequence)
      (let ((index (random (length sequence) random-state)))
	
	(multiple-value-bind (rest remainder) 
	    (pick-some (1- num) (append (subseq sequence 0 index) (subseq sequence (1+ index))))

	  (values (cons (elt sequence index) rest)
		  remainder)))
      (values nil sequence)))

(defun shuffle (sequence &optional (random-state *act-up-random-state*))
  (pick-some (length sequence) sequence random-state))
(defun noisy-shuffle (list noise &optional (random-state *act-up-random-state*))
  "Shuffles list.  List is changed by side-effect.
Noise [0,1] gives amount of shuffling. 
Returns shuffled list."
    (loop for i from 1 to (* 0.5 noise (length list)) do
	 
	 (let ((p1 (random (length list) random-state))
	       (p2 (random (length list) random-state)))
	   (let ((elt (nth p1 list)))
	     (setf (nth p1 list) (nth p2 list)
		   (nth p2 list) elt))))
    list)

(defun random-pareto (&optional xm k (random-state *act-up-random-state*))
  "Returns a random sample from a pareto distribution.
We're using inverse transform sampling to obtain the sample."
  (/ (or xm 1) (expt (random 1.0 random-state) (/ 1 (or k 1)))))


 
(export '(choice pick-some shuffle noisy-shuffle random-pareto))


;; calculate necessary noise

(defun actr-noise-for-activation-delta (x p)
  "This is derived from the cumulative distribution function
for a logistic distribution, with mu=0 (as in act-r-noise),
solved for S.  We provide the likelihood level p and the chosen activation level x.

I.e., this function will return s, leading to a logistic distribution yielding
samples that are `p' likely to fall below x.

p > 0.5 is a hard requirement, because s>0.  This may be understood given that
the noise distribution is two-tailed so that 0.5 is the maximum we can achieve
on one side of the mean."
  (if (> p 0.5)
      (- (/ x (log (- (/ 1 p) 1))))
      (error "p>0.5 (s>0) violation in parameter for noise.")))


(defun actr-noise-for-time (time1 time2 p)
"Calculcate noise s parameter to achieve a probability distribution 
of ((1-p),p) for the retrieval of two chunks (a,b),
a learned at t-time1, and a learned at t-time2.

p>0.5, because the more recent chunk will always be more likely to be retrieved.

e.g., for 180sec and 0sec, activation decay (0,5) causes a decay of 2.596.

however, if chunks were presented at t-380 and t-200, the decay would be 2.1036."

(let* ((decay (- (car (sgp :bll))))
       (mult 12)
       (act-delta (- (log (+ (* mult (expt time1 decay)) (* mult (expt time2 decay)))))))
 (actr-noise-for-activation-delta act-delta p)))


; In 1200 secs, decay (0.5) causes 0.0288 reduction in base-level activation

(defun actr-noise-unit-test ()

(loop for x in '(0.1 0.4 22 66) do
     (print "The following figures should be reasonably small.")
     (loop for p in '(0.6 0.8 0.95) do
	  (let ((sum 0) (num 0) (s (actr-noise-for-activation-delta x p)))
	    (print s)
	    (loop for c from 1 to 1000 do
		 (if (<= (act-r-noise s) x)
		     (incf num))
		 (incf sum))
	    ;;we probably want to print sdev here rather than mean(p-P)
	    (print (format nil "x=~a p=~a  \hat{p}=~a  mean (p-\hat{p})=~a" x p (/ num sum) (- (/ num sum) p)))))))
		 
  
;; other stuff
 

;; aggregation


(defparameter *aggregate-sum* nil)
(defparameter *aggregate-num* nil)
(defparameter *aggregate-colnames* nil)
(defun clear-aggregates (colnames)
  "Clear aggregation dataset.
COLNAMES is a list of symbols indicating the
names of variables that will be given as value(s)
and as conditions."
  (setq *aggregate-sum* nil 
	*aggregate-colnames* colnames))

(defun aggregate (value condition-list)
  "Add value to aggregation dataset.
VALUE may be a number or a sequence of numbers.
CONDITION-LIST is a sequence indicating the conditions that
VALUE will be associated with. 
VALUE is usually a dependent variable (measurement) obtained
when conditions CONDITION-LIST were present.
Aggregation will occur over all VALUEs in this combination of conditions."

  (if (assoc condition-list *aggregate-sum*  :test 'equal)
      (setf (cdr (assoc condition-list *aggregate-sum*  :test 'equal))
	    (cons value (cdr (assoc condition-list *aggregate-sum* :test 'equal))))
      (push (cons condition-list (list value))
	    *aggregate-sum*))
  nil)

(defun vec-op (operation v1 v2)
  (if (numberp v1)
      (if (numberp v2)
	  (if (and v1 v2)
	      (funcall operation v1 v2))
	  (loop for i2 in v2 collect (if (and v1 i2) (funcall operation v1 i2))))
      (if (numberp v2)
	  (loop for i1 in v1 collect (if (and i1 v2) (funcall operation i1 v2)))
	  (loop for i1 in v1 for i2 in v2 collect 
	       (if (and i1 i2) (funcall operation i1 i2))))))
  
; (vec-op '- '(4 1) '(5 2)) 

(defun aggregate-mean (condition-list)
  (aggregate-mean-2
   (assoc condition-list *aggregate-sum*  :test 'equal)))

(defun aggregate-mean-2 (values)
  (let ((sum (car values)))  ;; add up all vectors in values
    (loop for v in (cdr values) do
	 (setq sum (vec-op '+ v sum)))
    (vec-op '/ sum (length values))))

(defun aggregate-variance (condition-list)
  (aggregate-variance-2 
   (values (assoc condition-list *aggregate-sum*  :test 'equal))))

(defun aggregate-variance-2 (values)

  (let* ((mean (aggregate-mean-2 values))
	 (sum (vec-op '(lambda (x) (expt x 2))
				   (vec-op '- (car values)
					   mean))))
    (loop for v in (cdr values) do
	 (setq sum (vec-op '+ sum
			   (vec-op '(lambda (x) (expt x 2))
				   (vec-op '- v
					   mean)))))
    
    (vec-op 'sqrt (vec-op '/ sum (length values)))))
 



(defun agg-var-bucket-bindings (colnames variables bindings)
  (loop for c in colnames
     for b in bindings
     collect
       (if (member c variables)
	   b
	   nil)))

;; to interpret VARIABLES, we need to collect them first
;; the result looks like *aggregate-sum* structurally
(defun agg-aggregate-data (variables)
  (loop with buckets = nil
     for s in  (reverse *aggregate-sum*)
     for bucket-name = (agg-var-bucket-bindings *aggregate-colnames* variables (car s))
     do
       (if (assoc bucket-name buckets  :test 'equal)
	   (setf (cdr (last (assoc bucket-name buckets  :test 'equal)))
       		 (copy-list (cdr s)))
       	   (push (cons bucket-name  (copy-list (cdr s)))
       		 buckets))
     finally (return buckets)))

;; (defun aggtest (vars)
;;   (print (mapcar 'car  (agg-aggregate-data vars)))
;;   (equal  (agg-aggregate-data *aggregate-colnames*)  *aggregate-sum*))
;; (export '(aggtest))

(defun get-aggregates (&optional variables)
  "Return aggregation set as list of (conditions . means) conses."
 
  (when variables
    (loop for v in variables 
	 when (not (member v *aggregate-colnames*)) do
	 (error (format nil "print-aggregates: variable ~a was not declared with `clear-aggregates'" v))))

    (loop for s in (if variables (agg-aggregate-data variables) (reverse *aggregate-sum*))
       collect
	 (when (> (length (cdr s)) 0)
	   (cons (car s) 
		   (aggregate-mean-2 (cdr s))))))

(defun print-aggregates (&optional file variables)
  "Print aggregation set as a table.
Output is printed to FILE if given, standard out otherwise.
In this implementation, only mean values are printed.

If FILE is of the form (cond :append FILE2), then the output
will be appended to FILE2.

If VARIABLES is given, aggregate for combinations of
bindings for VARIABLES, and over all remaining variables.
Each variable must have been defined when `clear-aggregates' was
called."

  (when variables
    (loop for v in variables 
	 when (not (member v *aggregate-colnames*)) do
	 (error (format nil "print-aggregates: variable ~a was not declared with `clear-aggregates'" v))))

  (let ((append (if (and (consp file) (eq :append (car file))) 
		    :append
		    :supersede)))
    
    (if (consp file) (setq file (cadr file)))
    
    (let ((file-is-new (or (not file) (eq :supersede append) (not (probe-file file))))
	  (str (if file 
		   (open file
			 :direction :output :if-exists append :if-does-not-exist :create)
		   t)))
      (if file-is-new
	  (format str "~{~A~#[~:;~t~]~}~%" *aggregate-colnames*))
      
      (loop for s in (if variables (agg-aggregate-data variables) (reverse *aggregate-sum*))
	 do
	   (when (> (length (cdr s)) 0)
	     (format str "~{~A~#[~:;~t~]~}" (car s) )
	     (let ((mean (aggregate-mean-2 (cdr s))))
	       (cond ((floatp mean)
		      (if mean 
			  (format str " ~F" (float mean))
			  (format str " NA")))
		     ((numberp mean)
		      (format str " ~A" (if mean (float mean) "NA")))
		     (t 
		      (loop for i in mean do
			   (if (floatp i)
			       (format str " ~F" i)
			       (format str " ~A" (if i (float i) "NA") ))))))
	     (format str "~%")))
    (if file (close str)))))


;; (defun aggregate-test ()

;;   (clear-aggregates (list "resp"))
;;   (aggregate '(1 2) (list 'a))
;;   (aggregate '(2 3) (list 'a))
;;   (aggregate '(3.2 4) (list 'a))
;;   (print-aggregates))

(defun aggregate-test ()

  (clear-aggregates (list "beg-number"))
  (loop for cond from 5 to 10 do
       (loop for x from cond to 100 do
	    (aggregate (list x (expt x 2)) (list cond))))
  (print-aggregates))


;; (defun show-aggregates ()
;;   (show-aggregates-1 *aggregate-sum* *aggregate-num* nil))

;; (defun show-aggregates-1 (sum num filter)

;;   (cond ((= (length (caar sum)) 1)
;; 	 (loop for s in sum for n in num 
;; 	    do (format t "~a~t" (/ sum num)))
;; 	 (format t "~%"))
;; 	(t

;; 	 (let ((keys (loop for n in num
;; 		       collect
;; 			 (caar n))))
;; 	   (loop for key in keys do
;; 		(loop for s in sum for n in num do
		     
;; 	      (cons (cdr (car s))
;; 		    (cdr (car s))

;; )))))))



(export '(clear-aggregates aggregate print-aggregates get-aggregates))


;; Parameter optimization

(defparameter *optimize-parameters-verbose* t
  "If non-nil, `optimize-parameters' will print output.")

(defmacro optimize-parameters (variable-list &body body)
  "Optimize parameters from VARIABLE-LIST, running BODY.
Iterates through all combinations of values for variables in VARIABLE-LIST,
executing BODY for each.  BODY is expected to return a list of form
\(PARMS CORRELATION MEAN-DEV).  CORRELATION is the correlation coefficient,
MEAN-DEV is the Root Mean Squared Error of the parameter settings PARM
with the highest correlation.
VARIABLE-LIST is a list containing elements of form (NAME MIN MAX STEP)."

  ;; build loop expression
  (let* ((vlist (cons 'list (mapcar (lambda (s) `(cons ',(first s) ,(first s))) variable-list)))
	 (expr `(let ((perf (progn ,@body)))
		  (if (> (first perf) (first best-cor))
		      (setq best-cor perf
			    best-cor-par ,vlist))
		  (if (< (second perf) (second best-meandev))
		      (setq best-meandev perf
			    best-meandev-par ,vlist)))))
		 
    (loop for (name min max step) in variable-list do
	 (setq expr
	       `(loop for ,name from ,min to ,max by ,step
		   do ,expr)))
    (setq expr `(let ((best-cor '(0.0 0.0))
		      (best-cor-par nil)
		      (best-meandev '(0.0 1000.0))
		      (best-meandev-par nil))
		  ,expr
		  (when *optimize-parameters-verbose*
		    (format t "Corr:~,4F  RMSE:~,4F  at ~a~%"
			    (first best-cor) (second best-cor) best-cor-par)
		    (format t "Corr:~,4F  RMSE:~,4F  at ~a~%"
			    (first best-meandev) (second best-meandev) best-meandev-par))
		  (list best-cor-par (first best-cor) (second best-cor))))
    expr))

(provide "act-up-experiments")
