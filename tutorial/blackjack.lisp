;; -*-Mode: Act-up; fill-column: 75; comment-column: 50; -*-
;;; Filename: blackjack.lisp

;; To use: play-hands or run-blocks

;;; Author: Jasmeet Ajmani

;;; Acknowledgement: Dan Bothell

(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))

;; Architectural parameters
(setq *bll* 0.5
      *rt* -60
      *lf* 0.0
      *mp* 10.0)

;; Model parameters (set by `game0' and `game1')
(defparameter *opponent-threshold* 15)
(defparameter *deck1* 'regular-deck)
(defparameter *deck2* 'regular-deck)
(defparameter *opponent-rule* 'fixed-threshold)

;; declared special variables (for easy of inspection / debugging)

(defvar *card-list* nil)
(defvar *card* nil)

;;; test harness for the experiment

(defun play-hands (hands &optional (print-game nil))
  (number-sims)
  (let ((scores (list 0 0 0 0)))
   (dotimes (i hands)
      (let* ((mcards (deal *deck1*))
             (ocards (deal *deck2*))
             (mchoice (show-model-cards (butlast mcards) (first ocards)))  ;;; make show-model-cards return mchoice
             (ochoice (show-opponent-cards (butlast ocards) (first mcards))))
        
        (unless (eq 'hit mchoice) (setf mcards (butlast mcards)))
        (unless (eq 'hit ochoice) (setf ocards (butlast ocards)))
          
        (let* ((mtot (score-cards mcards))
               (otot (score-cards ocards))
               (mres (compute-outcome mcards ocards))
               (ores (compute-outcome ocards mcards)))
          
          (show-model-results mcards ocards mres ores)
          
          (when print-game
            (format t "Model: ~{~2d ~} -> ~2d (~4s)   Opponent: ~{~2d ~}-> ~2d (~4s)~%"
              mcards mtot mres ocards otot ores))
          
          (setf scores (mapcar #'+ scores
                         (list (if (eq mres 'win) 1 0)
                               (if (eq ores 'win) 1 0)
                               (if (and (eq mres 'bust) (eq ores 'bust)) 1 0)
                               (if (and (= mtot otot) (not (eq mres 'bust)) (not (eq ores 'bust))) 1 0)))))))
          scores))

(defun run-blocks (blocks block-size) 
  (let (res)    
    (dotimes (i blocks (reverse res))
      (push (play-hands block-size) res))))

(defun deal (deck)
  (list (funcall deck)
        (funcall deck)
        (funcall deck)))

(defun score-cards (list &optional (bust 21))
  (if (find 1 list)
      (special-score list bust)
    (apply #'+ list)))

(defun special-score (list bust)
  (let ((possible (list (apply #'+ list))))
    (dotimes (i (count 1 list))
      (push (+ (* 10 (1+ i)) (apply #'+ list)) possible))
    (apply 'max (remove-if (lambda (x) (> x bust)) possible))))
  
(defun compute-outcome (p1cards p2cards &optional (bust 21))
  (let ((p1tot (score-cards p1cards))
        (p2tot (score-cards p2cards)))
    (if (> p1tot bust) 
        'bust 
      (if (or (> p2tot bust) (> p1tot p2tot)) 
          'win 
        'lose))))   

(defun regular-deck ()
  (min 10 (1+ (random 13))))

(defun show-opponent-cards (cards mc1)
  (funcall *opponent-rule* cards mc1))

(defun fixed-threshold (cards mc1)
  (if (< (score-cards cards) *opponent-threshold*) 'hit 'stay)) 

(defun game0 ()
  (setf *deck1* 'regular-deck)
  (setf *deck2* 'regular-deck)
  (setf *opponent-threshold* 15)
  (setf *opponent-rule* 'fixed-threshold))

(defun game1 ()
  (setf *card-list* nil)
  (setf *deck1* 'stacked-deck)
  (setf *deck2* 'stacked-deck)
  (setf *opponent-rule* 'always-hit))

(defun load-stacked-deck ()
  (let* ((card1 (+ 5 (random 6)))
         (card2 (+ 5 (random 6)))
         (card4 (if (> (random 1.0) .5) 2 8))
         (card3 (if (= card4 2) 10 (- 21 (+ card1 card2))))
         (card5 10)
         (card6 (if  (= card4 2) 10 2)))
    (list card1 card2 card3 card4 card5 card6)))

(defun stacked-deck ()
  (cond (*card-list* (pop *card-list*))
        (t (setf *card-list* (load-stacked-deck)) 
           (pop *card-list*))))

(defun always-hit (cards mc1) 
  'hit)

(defun game2 ()
  (setf *deck1* 'sevens)
  (setf *deck2* 'sevens)
  (setf *opponent-threshold* 14)
  (setf *opponent-rule* 'fixed-threshold))

(defun sevens () 
  7)

(defun game3 ()
  (setf *deck1* 'high)
  (setf *deck2* 'high)
  (setf *opponent-rule* 'always-stay))

(defun high () 
  (+ 8 (random 3)))

(defun always-stay (cards mc1) 
  'stay)

(defun game4 ()
  (setf *card* 0)
  (setf *deck1* 'c246810)
  (setf *deck2* 'c246810)
  (setf *opponent-rule* 'always-hit))

(defun c246810 ()
  (setf *card* 
    (if (< *card* 10) 
        (+ 2 *card*) 
      2)))

(defun game5 ()
  (setf *card-list* nil)
  (setf *deck1* 'weird-deck)
  (setf *deck2* 'weird-deck)
  (setf *opponent-rule* 'random-hit))

(defun rand-sets () 
  (nth (random 6) 
       '((2 10 10) (4 9 9) (6 8 8) 
         (8 8 5) (9 9 3) (10 10 1))))

(defun weird-deck () 
  (cond (*card-list* (pop *card-list*))
        (t (setf *card-list* (rand-sets)) 
           (pop *card-list*))))

(defun random-hit (cards mc1) 
  (if (> (random 1.0) .5) 
      'hit 
    'stay))

(defun number-sims ()
  (set-similarities-fct 
   (loop for a from 1 to 30 append 
	 (loop for b from 1 to 30 
	       collect
	       (list a b (- (/ (abs (- a b)) (max a b))))))))

;;; defining chunk-types

(define-chunk-type game mtotal maction ocard result)

;;; defining procedures

(defproc show-model-cards (mcards ocard)
  (let* ((mc1 (first mcards))
	 (mc2 (second mcards))
	 (oc1 ocard)
	 (mstart (score-cards mcards))
	 (ostart (score-cards (list ocard)))
	 (mresult nil)
	 (oresult nil)
	 (model-action nil)
	 (g (retrieve-chunk (list :chunk-type 'game
				  :result 'win)
			    :soft-spec (list :mtotal mstart
					     :ocard oc1))))
    (if (eq g nil)
	(if (< 17 mstart)
	    (setf model-action 'stay)
	  (setf model-action 'hit))
      (setf model-action (game-maction g)))
    (pass-time 10)
    model-action))

(defproc show-model-results (mcards ocards mres ores)
  (let* ((mc1 (first mcards))
	 (mc2 (second mcards))
	 (mc3 (third mcards))
	 (mtot (score-cards mcards))
	 (mstart (score-cards (subseq mcards 0 2)))
	 (mresult mres)
	 (oc1 (first ocards))
	 (oc2 (second ocards))
	 (oc3 (third ocards))
	 (ostart (score-cards (list (first ocards))))
	 (oresult ores))
    (if (eq mc3 nil)
	(learn-chunk (make-game :mtotal mtot :maction 'stay :ocard oc1 :result mres))
      (learn-chunk (make-game :mtotal mtot :maction 'hit :ocard oc1 :result mres))))
  (pass-time 10))
      

    