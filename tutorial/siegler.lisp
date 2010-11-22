;; -*-Mode: Act-up; fill-column: 75; comment-column: 50; -*-

;; Siegler Model

;; paralleling the ACT-R 6 tutorial
;; translated to ACT-UP

;; Unlike the model in the tutorial, this model does not describe any
;; deterministic goings-on: aural module, encoding, decoding are left
;; out, because these steps do not contribute to the variance in the
;; data.  We achieve the same fit to the data.

;; run like this:

; (unit-test)

;; parameter fitting (demo)

; (fit-parameters)

;; Authors: D. Reitter / J. Ajmani
;; Carnegie Mellon University, 2010

;; (defpackage siegler (:use :common-lisp ))
;; (in-package siegler)

;; These load commands will find the ACT-UP files
;; relative to the location of the present file:
(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))
(load (concatenate 'string (directory-namestring *load-truename*) "../util/actr-stats.lisp"))


;; Architectural (ACT-R) parameters
(setq *rt* -.45
      *ans* .5 
      *bll* nil  ; defaults to nil in ACT-R
      *mp* 16)
      

(defvar *siegler-data* '((0   .05 .86  0  .02  0  .02  0   0  .06)
                         (0   .04 .07 .75 .04  0  .02  0   0  .09)
                         (0   .02  0  .10 .75 .05 .01 .03  0  .06)
                         (.02  0  .04 .05 .80 .04  0  .05  0   0)
                         (0    0  .07 .09 .25 .45 .08 .01 .01 .06)
                         (.04  0   0  .05 .21 .09 .48  0  .02 .11))
 "The experimental data to be fit")
(defparameter *correlation-siegler* nil)
(defparameter *meandev-siegler* nil)
(defvar *responses* nil)

;;;; chunk-types
(define-chunk-type plus-fact addend1 addend2 sum)
(defun num-name (num)
  (nth num '(zero one two three four five six seven eight nine ten)))
(defun name-num (name)
  (position name '(zero one two three four five six seven eight nine ten)))


(defun init-model ()
  (reset-model)
;;;; committing chunks to memory
  (loop for a from 0 to 5 do 
       (loop for b from 0 to 5 
	    when (< (+ a b) 10) do
	    (set-base-level-fct 
	     (learn-chunk (make-plus-fact :name (intern (format nil "~a+~a" a b))
					  :addend1 (num-name a)
					  :addend2 (num-name b) 
					  :sum (num-name (+ a b))))
			  (if (< (+ a b) 5) 0.65 0.0))))
  (set-similarities-fct 
   (loop for a from 0 to 5 append 
	(loop for b from (1+ a) to 5 when (< (+ a b) 10) collect
	     (list (num-name a) (num-name b) (- (float (/ (abs (- a b)) 10))))))))
  

;;;; defining productions

(defun unit-test()
  (run-subjects 1000))

(defun test-one-fact (arg1 arg2)
  "Run the model once."
  (init-model)
  (test-fact arg1 arg2))

(defun do-one-set ()

  (init-model)
  (list (test-one-fact 'one 'one)
        (test-one-fact 'one 'two)
        (test-one-fact 'one 'three)
        (test-one-fact 'two 'two)
        (test-one-fact 'two 'three)
        (test-one-fact 'three 'three)))


(defun run-subjects (n)
  (let ((responses nil))
    (dotimes (i n)
      (push (do-one-set) responses))
    (setq *responses* responses)
    (analyze responses))
  (list *correlation-siegler* *meandev-siegler*))

(defun aggregate-responses (responses)
  (mapcar (lambda (x) 
             (mapcar (lambda (y) 
                       (/ y (length responses))) x))
     (apply #'mapcar 
            (lambda (&rest z) 
              (let ((res nil))
                (dolist (i '(zero one two three four five six seven eight))
                  (push (count i z ) res)
                  (setf z (remove i z )))
                (push (length z) res)
                (reverse res)))
            responses)))

(defun analyze (responses)
  (display-results 
   (aggregate-responses responses)))

(defun display-results (results)
  (let ((questions '("1+1" "1+2" "1+3" "2+2" "2+3" "3+3")))
    (setq *correlation-siegler* (correlation results *siegler-data*))
    (setq *meandev-siegler* (mean-deviation results *siegler-data*))
    (format t "       0     1     2     3     4     5     6     7     8   Other~%")
    (dotimes (i 6)
      (format t "~a~{~6,2f~}~%" (nth i questions) (nth i results)))))

(defun fit-parameters ()

  (loop for *mp* from 12.0 to 18.0 by 1.0
       do
       (loop for *ans* from 0.2 to 0.6 by 0.1 do
	    (let ((responses nil))
	      (dotimes (i 100)
		(push (do-one-set) responses))
	      (let ((results (aggregate-responses responses)))
		(format t "                                  mp:~a  ans:~a   c:~a     d:~a  ~%"
			*mp* *ans*
			(correlation results *siegler-data*)
			(mean-deviation results *siegler-data*)))))))

(defun output-responses ()
  (format t "~{~a ~}~%" '("c11" "c12" "c13" "c22" "c23" "c33"))
  (loop for s in *responses* do
      (format t "~{~a ~}~%"
	      (mapcar 'name-num s))))

;; R code for plotting the data and the results:
;; d = read.table("siegler-responses.txt", header=T, colClasses=c("numeric","numeric","numeric","numeric","numeric","numeric"), na.strings=c("NIL"))
;; ds = read.table("siegler-actr-responses.txt", header=T, colClasses=c("numeric","numeric","numeric","numeric","numeric","numeric"), na.strings=c("NIL"))
;; plot(density(d$c12,na.rm=T,n=18,from=0, to=8, kernel="g",adjust=10))


;; The Model

(defproc test-fact (arg1 arg2)
  ;; encoding /aural is deterministic in ACT-R
  (pass-time 1.0)
  (let ((cfd  (retrieve-chunk (list :chunk-type 'plus-fact)
			      :soft-spec (list
					  :addend1 arg1 :addend2 arg2))))
    (pass-time 0.4)
    (if cfd (plus-fact-sum cfd))))
