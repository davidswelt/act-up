;; -*-Mode: Act-up; fill-column: 75; comment-column: 50; -*-
;;; Filename: past-tense.lisp

;;; To run use command: (do-it)

;;; Author: Jasmeet Ajmani, David Reitter, Dan Bothell


(declaim (optimize (speed 0) (space 0) (debug 03)))

;; These load commands will find the ACT-UP file
;; relative to the location of the present file:
(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))

;; Architectural (ACT-R) parameters

(setq *rt* 0.5)
(setq *bll* 0.5)
(setq *ans* 0.1)
(setq *lf* 0.5)
(setq *blc* 0.0)
(setq *egs* 0.2)
(setq *mas*  3.5)
(setq *rt* 0.5)
(setq *procedure-compilation* t)
(setq *alpha* 0.1)
(setq *iu* 5)
(setq *ol* 6)

(defvar *report*)

(defun make-triples (l)
  (when l
    (cons (list (first l)
		(second l)
		(third l)
		(fourth l)
		(if (eq (second l) 'I) 'blank 'ed))
	  (make-triples (nthcdr 4 l)))))

(defparameter *word-list* 
  (make-triples '(have      I              12458 had
                  do        I               4367 did
                  make      I               2312 made
                  get       I               1486 got
                  use       R               1016 use
                  look      R                910 look
                  seem      R                831 seem
                  tell      I                759 told
                  show      R                640 show
                  want      R                631 want
                  call      R                627 call
                  ask       R                612 ask
                  turn      R                566 turn
                  follow    R                540 follow
                  work      R                496 work
                  live      R                472 live
                  try       R                472 try
                  stand     I                468 stood
                  move      R                447 move
                  need      R                413 need
                  start     R                386 start
                  lose      I                274 lost)))


(defparameter *total-count* (apply #'+ (mapcar #'third *word-list*)))

;;; Select a random word from the vocabulary, but based on its frequency

(defun random-word ()
  (let
    ((num (random *total-count*)))
    (dolist (i *word-list*)
      (if (< num (third i))
        (return i)
        (setf num (- num (third i)))))))

;;;  Run the model

(defun run-model-once ()
  (let* ((wordpair (random-word))
	 (word (first wordpair)))
    (list (past-tense-model word)  ;; run model
	  word
	  (second wordpair))))

(defun rep-f-i (l n)
   (if l
     (let ((x (if (> (length l) n) (subseq l 0 n) l))
           (y (if (> (length l) n) (subseq l n) nil))
           (irreg 0)
           (reg 0)
           (none 0)
           (data nil))
       (dolist (i x)
         (cond ((eq (first i) 'R) nil)
               ((eq (second i) 'reg) (setf reg (1+ reg)))
               ((eq (second i) 'irreg) (setf irreg (1+ irreg)))
               (t (setf none (1+ none)))))
       (if (> (+ irreg reg none) 0)
         (setf data (list (/ irreg (+ irreg reg none))
                 (/ reg (+ irreg reg none))
		 (/ none (+ irreg reg none))
                 (if (> (+ irreg reg) 0) (/ irreg (+ irreg reg)) 0)))
         (setf data (list 0 0 0 0)))
       (format t "~{~6,3F~}~%" data)
       (cons data (rep-f-i y n)))
    nil))

;;; The following function reports how often an irregular word gets an irregular 
;;; (correct), regular past tense or just the stem as past tense (None).

(defun add-to-report (word-stem-suffix word number)
   (let ((word (first word-stem-suffix))
	 (stem (second word-stem-suffix))
	 (suffix (third word-stem-suffix)))
     (cond
      ((eq stem word) (push (list number 'reg word) *report*))
      ((eq suffix nil) (push (list number 'none word) *report*))
      (t (push (list number 'irreg word) *report*)))))

;;; This function will run the experiment for n trials.
;;; The keyword parameters are:
;;;   repfreq - determines how often results are reported during a run.


(defun do-it (&optional (n 20000) (repfreq 1000))
    (init-model)
    (format t "~%trial   irreg   reg  none  irreg.prop  ~%")
    (setf *report* nil)
    (let ((trial 0) (repcount 0))
      (dotimes (i n)
	;; add two random words to memory
	(add-past-tense-to-memory)
	(add-past-tense-to-memory)
	(apply #'add-to-report (run-model-once))
	(incf repcount)
	(when (>= repcount repfreq)
	  (format t "~6D  " (1+ trial))
	  (rep-f-i (subseq *report* 0 repfreq) repfreq)
	  (setf repcount 0))
	(incf trial))))

(defun plot-it (&optional (n 20000) (repfreq 1000))

  (with-open-file (*standard-output* "past-tense-results.txt" :direction :output
			  :if-exists :supersede)
    (do-it n repfreq))
  (run-program "/usr/bin/R" '("CMD" "BATCH" "past-tense.R") :output t)
  (run-program "/usr/bin/open" (list "past-tense.pdf")))
	 

;;;; define chunk type
(define-chunk-type pasttense verb stem suffix)

;;;; committing chunks to memory
;;;; no suffix means irregular
;;;; This function simulates "hearing" a past tense: it adds a correct 
;;;; past tense to memory.

(defun init-model ()
  (reset-model))

;;  work      R                496 work

(defun add-past-tense-to-memory ()
   (let*
     ((wordpair (random-word))
      (word (first wordpair))  ;; "work"
      (stem (fourth wordpair))  ;; "work" 
      (suffix (fifth wordpair)))  ;; blank / ed
     (learn-chunk (make-pasttense* :verb word :stem stem :suffix suffix)))
   (pass-time 0.05))


;; (defun add-past-tense-to-memory 
  
;;   (loop for (verb stem suffix) in
;;        '((have had blank)
;; 	 (do did blank)
;; 	 (make made blank)
;; 	 (stand stood blank)
;; 	 (move move ed)
;; 	 (need need ed)
;; 	 (start start ed)
;; 	 (lose lost blank)
;; 	 (use use ed))
;;        do
;;        (if init
;; 	   (learn-chunk (make-pasttense :name verb :verb verb :stem stem :suffix suffix))
;; 	   (learn-chunk verb))
;;        (pass-time 0.05)))

;;;; The model


(defproc ptmodel (word)
  "Form past-tense of WORD."
  :group past-tense-model
  (let ((q (form-past-tense word)))

   ; (if q (print 'ok) (print 'fail))
    (pass-time *model-time-parameter*)
    (if q
	(if (eq (third q) 'blank)
	    (assign-reward 5.0)
	    (assign-reward 4.2))
	(assign-reward 3.9))
    (if q (learn-chunk (make-pasttense* 
			    :verb (first q)
			    :stem (second q)
			    :suffix (third q)
			    )))
    q))

;;; Strategies 
;;; All of them take a word as input and
;;; return a list with verb, stem, and suffix.

(defparameter *model-time-parameter* 1.5)

(defproc strategy-by-retrieval (word)
  "Retrieve memorized past tense form for WORD."
  :group form-past-tense
  (let ((dec (retrieve-chunk (list :chunk-type 'pasttense :verb word :suffix 'non-nil))))
    (when dec  ;; retrieved?
      (list word (pasttense-stem dec) (pasttense-suffix dec)))))

(defproc try-analogy-and-get-same-word (word)
  "Retrieve some past tense and got lucky."
  :group form-past-tense
  (let ((dec (retrieve-chunk (list :chunk-type 'pasttense :suffix 'non-nil))))
    (when (and dec (eq word (pasttense-verb dec))) ;; got the right word
      (list word (pasttense-stem dec) (pasttense-suffix dec)))))

(defproc strategy-with-analogy (word)
  "Retrieve some past tense form, using analogy."
  :group form-past-tense
  (let ((dec (retrieve-chunk (list :chunk-type 'pasttense :suffix 'non-nil))))
    (when (and dec (eq (pasttense-verb dec) (pasttense-stem dec))) ;; an analogy is possible
      (list word word (pasttense-suffix dec)))))