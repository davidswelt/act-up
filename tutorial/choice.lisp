(require "act-up" "../act-up.lisp")
(use-package :act-up)


(defun do-set ()
  (reset-model)
  (loop for s from 1 to 4
	collect
	(/
	 (loop for i from 1 to 12
	       sum
	       (if (choice) 1 0))
	 12.0)))

	    
(defun choice ()   
  (let ((ch (choose-coin))
	(r (if (< (random 1.0) .9) 'heads 'tails)))
    (print ch)
    (cond
     ((eq r ch)
      (process-result t)
      t)
     ((not (eq r ch))
      (process-result nil)
      nil))))

(defrule decide-tails ()
  :group choose-coin
  'tails)

(defrule decide-heads ()
  :group choose-coin
  'heads)

(defrule process-result (correct)
  (assign-reward (if correct 2.0 0.0)))
     


