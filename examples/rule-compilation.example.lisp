;; 
(require "act-up" "../act-up.lisp")
(use-package :act-up)


(define-chunk-type mod-fact val mod result)
(define-chunk-type plus-fact addend1 addend2 sum)

(defun init-model ()
  (reset-model)
  (loop for a from 0 to 20 do 
	    (set-base-level-fct 
	     (learn-chunk (make-mod-fact :name (intern (format nil "~amod10" a ))
					  :val a
					  :mod 10 
					  :result (mod a 10)))
	     0.3))
  (loop for a from 0 to 10 do 
       (loop for b from 0 to 10 do
	    (set-base-level-fct 
	     (learn-chunk (make-plus-fact :name (intern (format nil "~a+~a" a b))
					  :addend1 a
					  :addend2 b 
					  :sum (+ a b)))
	     (if (< (+ a b) 5) 0.65 0.4)))))



(defrule plus3mod10 (c)
  :group plus3mod10-group
  "This rule computes c+x % 10."
  (mod-rule
   (plus-rule 3 c) 10))

(defrule plus-rule (c d)
  "Plus"
  :group plus-group
  "c+d"
  (plus-fact-sum
   (learn-chunk (retrieve-chunk (list :chunk-type 'plus-fact :addend1 c :addend2 d)))))

(defrule mod-rule (c m)
  :group mod-group
  "c%10"
  (mod-fact-result
   (learn-chunk (retrieve-chunk (list :chunk-type 'mod-fact :val c :mod m)))))


(defun run ()
  (let ((ok nil)
	(x (random 11)))
	(print (stop-actup-time
		 (when (= (mod (+ x 3) 10)
			(plus3mod10-group x))
		   (setq ok t)
		   (assign-reward 1.0))))
	ok
))
(defun run-all ()
  (init-model)
  (let ((good 0))
    (dotimes (n 1000)
      (if (run)
	  (incf good))
      )
    good))
  
  