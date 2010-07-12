(require "act-up" "act-up.lisp")
(use-package :act-up)
(load "actr-stats")

(setq *rule-compilation* t)
(setq *rt* 0.5)
(setq *bll* 0.5)
(setq *ans* 0.1)
(setq *lf* 0.5)
(setq *blc* 0.5)

(defvar *report*)
(defvar *number*)
(defvar *word*)
(defvar *repcount*)
(defvar *trial*)

(defun make-triples (l)
   (when l
   (cons (list (first l)(second l)(third l)
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

;;; Set the goal to do one past tense

(defun make-one-goal ()
  (let*
      ((wordpair (random-word))
       (word (first wordpair))
       (word-no (second wordpair))
       (m nil))
    (progn
      (setf *number* word-no *word* word)
      (setq m (ptmodel word)))
    m))

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
                 (/ reg (+ irreg reg none))(/ none (+ irreg reg none))
                 (if (> (+ irreg reg) 0) (/ irreg (+ irreg reg)) 0)))
         (setf data (list 0 0 0 0)))
       (format t "~{~6,3F~}~%" data)
       (cons data (rep-f-i y n)))
    nil))

;;; The following function reports how often an irregular word gets an irregular 
;;; (correct), regular past tense or just the stem as past tense (None).

(defun add-to-report (the-chunk)
   (let* ((stem (second the-chunk))
          (word (first the-chunk))
          (suffix (third the-chunk)))
     (cond
      ((eq stem word) (push (list *number* 'reg *word*) *report*))
      ((eq suffix nil) (push (list *number* 'none *word*) *report*))
      (t (push (list *number* 'irreg *word*) *report*)))))

;;; This function will run the experiment for n trials.
;;; The keyword parameters are:
;;;   repfreq - determines how often results are reported during a run.


(defun do-it (n &optional (repfreq 100))
    (init-model)
    (format t "~%")
    (setf *report* nil)
    (setf *trial* 0 *repcount* 0)
    (dotimes (i n)
      (add-past-tense-to-memory)
      (add-past-tense-to-memory)
      (add-to-report (make-one-goal))
    (incf *repcount*)
    (when (>= *repcount* repfreq)
      (format t "Trial ~6D : " (1+ *trial*))
      (rep-f-i (subseq *report* 0 repfreq) repfreq)
      (setf *repcount* 0))
    (incf *trial*)))

;;;; define chunk type
(define-chunk-type pasttense verb stem suffix)

;;;; committing chunks to memory
;;;; no suffix means irregular
;;;; This function simulates "hearing" a past tense: it adds a correct 
;;;; past tense to memory.

(defun init-model ()
  (reset-model)
  (add-past-tense-to-memory 'initialize))

(defun add-past-tense-to-memory (&optional init)
  
  (loop for (verb stem suffix) in
       '((have had blank)
	 (do did blank)
	 (make made blank)
	 (stand stood blank)
	 (move move ed)
	 (need need ed)
	 (start start ed)
	 (lose lost blank)
	 (use use ed))
       do
       (if init
	   (learn-chunk (make-pasttense :name verb :verb verb :stem stem :suffix suffix))
	   (learn-chunk verb))))

;;;; Defining productions

(defrule ptmodel (word)
  (let* ((q (choose-strategy word))
	 (ans (third q)))
    (progn
      (if (eq ans nil)
	  (assign-reward 3.9)
	(if (eq ans 'blank)
	    (assign-reward 5.0)
	  (assign-reward 4.2))))
    q))

;;; make strategies return list verb stem suffix

(defrule strategy-without-analogy (word)
  :group choose-strategy
  (let* ((dec (retrieve-chunk (list :chunk-type 'pasttense :verb word)))
	 (verb word)
	 (stem nil)
	 (suffix nil))
    (if (not (eq dec nil))
      (progn
	(learn-chunk dec)
	(setq verb word)
	(setq stem (pasttense-stem dec))
	(setq suffix (pasttense-suffix dec))))
    (list verb stem suffix)))

(defrule strategy-with-analogy (word)
  :group choose-strategy
  (let* ((dec (retrieve-chunk (list :chunk-type 'pasttense)))
	 (verb word)
	 (stem nil)
	 (suffix nil))
    (if (not (eq dec nil))
      (progn
	(learn-chunk dec)
	(setq verb word)
	(setq stem (pasttense-stem dec))
	(setq suffix (pasttense-suffix dec))))
    (list verb stem suffix)))

(reset-model)