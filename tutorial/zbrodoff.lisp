;;; Filename: zbrodoff.lisp

;;; To run use command: (collect-data 100)

;;; Author: David Reitter/ Jasmeet Ajmani
;;; Acknowledgements: Dan Bothell

;; we use a more complex notation to find the ACT-UP file
;; relative to the location of the tutorial file.
(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))
(load (concatenate 'string (directory-namestring *load-truename*) "../util/actr-stats.lisp"))

(setq *bll* 1000)                  
(setq *lf* 0.4)   
(setq *ans* 0.5)
(setq *rt* -5)
(setq *blc* 10.0)

(defvar *trials*)
(defvar *results*)
(defvar *start-time*)
(defvar *block*)

(defvar *zbrodoff-control-data* '(1.84 2.46 2.82 1.21 1.45 1.42 1.14 1.21 1.17))

(defstruct trial block addend1 addend2 sum answer)
(defstruct response block addend correct time)

;;;; Test harness for the experiment

(defun collect-responses ()
  (setf *start-time* (actup-time))
  (setf trial (first *trials*))
  (setf var (increase-letter (trial-addend1 trial) (trial-addend2 trial)))
  (setq key (if (eq var (trial-sum trial)) 'K 'D))
  (let ((trial (pop *trials*)))
    (push (make-response :block (trial-block trial)
                         :addend (write-to-string (trial-addend2 trial))
                         :time (- (actup-time) *start-time*)
                         :correct (eq (trial-answer trial) key))
          *results*)
    (when *trials*
      (collect-responses))))


(defun do-experiment ()
  (setf *trials* nil)
  (dotimes (j 3)
    (setf *block* (+ j 1))
    (dotimes (i 8)
      (setf *trials* (append *trials* (create-set)))))
  (collect-responses)
  (analyze-results))

(defun collect-data (n)
  (init-model)
  (setf *results* nil)
  (let ((results nil))
    (dotimes (i n)
      (push (do-experiment) results))
    
    (let ((rts (mapcar #'(lambda (x) (/ x (length results)))
                 (apply #'mapcar #'+ (mapcar #'first results))))
          (counts (mapcar #'(lambda (x) (truncate x (length results)))
                    (apply #'mapcar #'+ (mapcar #'second results)))))
      
      (correlation rts *zbrodoff-control-data*)
      (mean-deviation rts *zbrodoff-control-data*)
      
      (print-analysis rts counts '(1 2 3) '("2" "3" "4") '(64 64 64)))))

        
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
	  '((a 2 c k)(d 2 f k)
	    (b 3 e k)(e 3 h k)
	    (c 4 g k)(f 4 j k)
	    (a 2 d d)(d 2 g d)
	    (b 3 f d)(e 3 i d)
	    (c 4 h d)(f 4 k d)
	    (a 2 c k)(d 2 f k)
	    (b 3 e k)(e 3 h k)
	    (c 4 g k)(f 4 j k)
	    (a 2 d d)(d 2 g d)
	    (b 3 f d)(e 3 i d)
	    (c 4 h d)(f 4 k d))))

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
  (learn-chunk (make-seqfact :identity '0 :next '1))
  (learn-chunk (make-seqfact :identity '1 :next '2))
  (learn-chunk (make-seqfact :identity '2 :next '3))
  (learn-chunk (make-seqfact :identity '3 :next '4))
  (learn-chunk (make-seqfact :identity '4 :next '5))
  (learn-chunk (make-seqfact :identity 'a :next 'b))
  (learn-chunk (make-seqfact :identity 'b :next 'c))
  (learn-chunk (make-seqfact :identity 'c :next 'd))
  (learn-chunk (make-seqfact :identity 'd :next 'e))
  (learn-chunk (make-seqfact :identity 'e :next 'f))
  (learn-chunk (make-seqfact :identity 'f :next 'g))
  (learn-chunk (make-seqfact :identity 'g :next 'h))
  (learn-chunk (make-seqfact :identity 'h :next 'i))
  (learn-chunk (make-seqfact :identity 'i :next 'j))
  (learn-chunk (make-seqfact :identity 'j :next 'k)))

;;;; Defining Procedural Rule

(defproc increase-letter (first-letter target-i &optional (letter first-letter) (i 0))
  (if (= target-i i)
      letter
    (let* ((stored-solution (retrieve-chunk (list :chunk-type 'problem :arg1 first-letter :arg2 target-i))))
      (if stored-solution   ; found
	  (progn
	    (pass-time 0.96)
	    ;(pass-time (* 4 *one-step-duration*))
	    (learn-chunk stored-solution)
	    (problem-r stored-solution))
	;; not found:
	(progn
	  (pass-time 2.2)
	  ;(pass-time (* 11 *one-step-duration*))
	(let* ((next-i (seqfact-next (retrieve-chunk (list :chunk-type 'seqfact :identity i)))) 
	       (next-letter (seqfact-next (retrieve-chunk (list :chunk-type 'seqfact :identity letter)))))
	  (learn-chunk (make-problem :arg1 first-letter :arg2 next-i :r next-letter))
	  (increase-letter first-letter target-i next-letter next-i)))))))

(init-model)