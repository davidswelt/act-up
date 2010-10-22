;;; Filename: zbrodoff.lisp

;;; To run use command: (collect-data 100)

;;; Author: David Reitter/ Jasmeet Ajmani
;;; Acknowledgements: Dan Bothell

;; we use a more complex notation to find the ACT-UP file
;; relative to the location of the tutorial file.
(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))
(load (concatenate 'string (directory-namestring *load-truename*) "../util/actr-stats.lisp"))


;; Architectural parameters
(setq *bll* 0.5)                  
(setq *lf* 1.0)
(setq *ans* 0.5)
(setq *rt* 1.2)
(setq *blc* 0.0)

;; Model parameters:
(defparameter *model-time-parameter-1* 0.55)
(defparameter *model-time-parameter-2* 0.54)


;; Alternative, using *lf* from the ACT-R Tutorial:
;; (setq *lf* 0.4)
;; (defparameter *model-time-parameter-1* 0.6)
;; (defparameter *model-time-parameter-2* 0.8)



(defvar *trials*)
(defvar *results*)
(defvar *block*)

(defvar *zbrodoff-control-data* '(1.84 2.46 2.82 1.21 1.45 1.42 1.14 1.21 1.17))

(defstruct trial block addend1 addend2 sum answer)
(defstruct response block addend correct time)

;;;; Test harness for the experiment

(defun collect-responses ()
  (setf trial (first *trials*))
  (let* ((key nil)
	 (duration 
	  (stop-actup-time
	    (setq key (if (eq (trial-sum trial)
			      (increase-letter (trial-addend1 trial) (trial-addend2 trial)))
			  'K 'D)))))
    (let ((trial (pop *trials*)))
      (push (make-response :block (trial-block trial)
			   :addend (write-to-string (trial-addend2 trial))
			   :time duration
			   :correct (eq (trial-answer trial) key))
	    *results*)))
  (when *trials*
    (collect-responses)))


(defun do-experiment ()
  (init-model)
  (setf *trials* nil)
  (dotimes (j 3)
    (setf *block* (+ j 1))
    (dotimes (i 8)
      (setf *trials* (append *trials* (create-set)))))
  (collect-responses)
  (analyze-results))

(defun collect-data-silently (n)
  (setf *results* nil)
  (let ((results nil))
    (dotimes (i n)
      (push (do-experiment) results))
    
    (let ((rts (mapcar #'(lambda (x) (/ x (length results)))
                 (apply #'mapcar #'+ (mapcar #'first results)))))
      
      
      (list (correlation rts *zbrodoff-control-data*)
	    (mean-deviation rts *zbrodoff-control-data*)))))

(defun collect-data (n)

  (setf *results* nil)
  (let ((results nil))
    (dotimes (i n)
      (init-model)
      (push (do-experiment) results))
    
    (let ((rts (mapcar #'(lambda (x) (/ x (length results)))
                 (apply #'mapcar #'+ (mapcar #'first results))))
          (counts (mapcar #'(lambda (x) (truncate x (length results)))
                    (apply #'mapcar #'+ (mapcar #'second results)))))
      
      
      (print-analysis rts counts '(1 2 3) '("2" "3" "4") '(64 64 64))
      (list (correlation rts *zbrodoff-control-data*)
	    (mean-deviation rts *zbrodoff-control-data*)))))
        
(defun analyze-results ()
  (let ((blocks (sort (remove-duplicates (mapcar #'response-block *results*)) #'<))
        (addends (sort (remove-duplicates (mapcar #'response-addend *results*) :test #'string-equal) #'string<))
        (counts nil)
        (rts nil)
        (total-counts nil))
    (setf total-counts (mapcar #'(lambda (x) 
                                   (/ (count x *results* 
                                             :key #'response-addend 
                                             :test #'string=)
                                      (length blocks)))
                         addends))
    
    (dolist (x blocks)
      (dolist (y addends)
        (let ((data (mapcar #'response-time
                      (remove-if-not #'(lambda (z)
                                         (and (response-correct z)
                                              (string= y (response-addend z))
                                              (= x (response-block z))))
                                     *results*))))
          (push (length data) counts)
          (push (/ (apply #'+ data) (max 1 (length data))) rts))))
      
    (list (reverse rts) (reverse counts))))

    
(defun print-analysis (rts counts blocks addends total-counts)
  (format t "~%        ")
  (dotimes (addend (length addends))
    (format t " ~6@a (~2d)" (nth addend addends) (nth addend total-counts)))
  (dotimes (block (length blocks))
    (format t "~%Block ~2d" (nth block blocks))
    (dotimes (addend (length addends))
      (format t " ~6,3f (~2d)" (nth (+ addend (* block (length addends))) rts)
        (nth (+ addend (* block (length addends))) counts))))
  (terpri))
        
  
(defun create-set ()
  (mapcar #'construct-trial 
	  '((a 2 c k) (d 2 f k)
	    (b 3 e k) (e 3 h k)
	    (c 4 g k) (f 4 j k)
	    (a 2 d d) (d 2 g d)
	    (b 3 f d) (e 3 i d)
	    (c 4 h d) (f 4 k d)
	    (a 2 c k) (d 2 f k)
	    (b 3 e k) (e 3 h k)
	    (c 4 g k) (f 4 j k)
	    (a 2 d d) (d 2 g d)
	    (b 3 f d) (e 3 i d)
	    (c 4 h d) (f 4 k d))))

(defun construct-trial (settings)
  (make-trial :block *block* 
              :addend1 (first settings)
              :addend2 (second settings)
              :sum (third settings)
              :answer (fourth settings)))


;;;; Define chunk types

(define-chunk-type problem arg1 arg2 r)
(define-chunk-type seqfact identity next)

;;;; committing chunks to memory

(defun init-model ()
  (reset-model)

  ;; create seqfact chunks for (0 1), (3 4), (i j) etc:
  (loop for (a b) on '(0 1 2 3 4 5 a b c d e f g h i j k) 
       when b
       do
       (set-base-level-fct
	(learn-chunk (make-seqfact :identity a :next b))
	100000 -1000)))

;;;; Defining Procedural Rule

(defproc increase-letter (first-letter target-i &optional (letter first-letter) (i 0))
  (if (= target-i i)
      letter
    (let* ((stored-solution (retrieve-chunk (list :chunk-type 'problem :arg1 first-letter :arg2 target-i))))
      (if stored-solution   ; found
	  (progn
	    (pass-time *model-time-parameter-1*)
	    ;(pass-time (* 4 *one-step-duration*))
	    (learn-chunk stored-solution)
	    (problem-r stored-solution))
	;; not found:
	(progn
	  (pass-time *model-time-parameter-2*)
	  ;(pass-time (* 11 *one-step-duration*))
	(let (retrieved-seqfact next-i next-letter)
	  (and  ; this ensures that we're successfully retrieving each chunk (r)
	   (setq retrieved-seqfact (retrieve-chunk (list :chunk-type 'seqfact :identity i)))
	   (setq next-i (seqfact-next retrieved-seqfact))
	   (setq retrieved-seqfact (retrieve-chunk (list :chunk-type 'seqfact :identity letter)))
	   (setq next-letter (seqfact-next retrieved-seqfact))
	   (learn-chunk (make-problem* :arg1 first-letter :arg2 next-i :r next-letter))
	   (increase-letter first-letter target-i next-letter next-i))))))))

(init-model)



(defun optimize-parameters ()
  (with-open-file (output "zbrodoff-results.txt" :direction :output
			  :if-exists :supersede)
    (format output "time1~ttime2~tcorrelation~tdeviation~%")
    (let ((minimum 0.4)
	  (maximum 1.3)
	  (best-cor 0.0)
	  (best-cor-par nil)
	  (best-meandev 1000.0)
	  (best-meandev-par nil)
	  (best-tradeoff 0)
	  (best-tradeoff-par nil)
	  ;; do not print extra output:
	  (*actr-stats-show-results* nil))
    (loop with *actr-stats-show-results* = nil
       for *model-time-parameter-1* from minimum to maximum by 0.1 do
	 (print *model-time-parameter-1*) (terpri)
	 (loop for *model-time-parameter-2* from minimum to maximum by 0.05
	    for rts = (collect-data-silently 15)
	    do
	      (let* (
		    (cor (first rts))
		    (meandev (second rts)))
		(if (> cor best-cor)
		    (setq best-cor cor
			  best-cor-par (list *model-time-parameter-1* *model-time-parameter-2*)))
		(if (< meandev best-meandev)
		    (setq best-meandev meandev
			  best-meandev-par (list *model-time-parameter-1* *model-time-parameter-2*)))
		(if (> (- cor meandev) best-tradeoff)
		    (setq best-tradeoff (- cor meandev)
			  best-tradeoff-par (list *model-time-parameter-1* *model-time-parameter-2*)))

		(format output "~,4F~t~,4F ~t~,4F~t~,4F~%" 
			*model-time-parameter-1* *model-time-parameter-2*
			cor
			meandev))))

    (format t "Best correlation: ~,4F at parameters ~a~%"
	    best-cor best-cor-par)
    (format t "Best mean deviation: ~,4F at parameters ~a~%"
	    best-meandev best-meandev-par)
    (format t "Best tradeoff (?): ~,4F at parameters ~a~%"
	    best-tradeoff best-tradeoff-par))))



;; With the ACT-R default parameters (*lf*=1.0)
;; Best correlation: 0.9972 at parameters (0.4 0.41)
;; Best mean deviation: 0.0549 at parameters (0.5499999 0.5399999)
;; manual selection (via plots, and in R)
;; suggests 0.55 and 0.54, giving these:
;; CORRELATION:  0.994
;; MEAN DEVIATION:  0.066

;; With the LF parameter from the ACT-R tutorial (*lf*=0.4)
;; Best correlation: 0.9971 at parameters (0.4 0.45000002)
;; Best mean deviation: 0.0775 at parameters (0.6 0.8000001)
;; CORRELATION:  0.992
;; MEAN DEVIATION:  0.095


;; R code to plot and inspect the results:
;; d <- read.table("zbrodoff-results.txt", header=T)
;; library (Hmisc)  # may need to be installed!
;; xYplot(deviation~time1+time2, data=d, method=smean.cl.boot)
;; xYplot(deviation~I(time1+time2), data=d, method=smean.cl.boot)
;; d2 = subset(d, time1+time2>1.07 & time1+time2<1.12)
;; subset(d2, correlation==max(d2$correlation))