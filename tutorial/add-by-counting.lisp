;;; Filename: add-by-counting.lisp

;;; Author: David Reitter

;; we use a more complex notation to find the ACT-UP file
;; relative to the location of the tutorial file.
(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))

;; ACT-R parameters
(setq *lf* .05)
(setq *rt* -1)

;;;; Defining chunk type

(define-chunk-type count-order first second)

;;;; Committing chunks to memory

(learn-chunk (make-count-order :name 'one-two :first 1 :second 2))
(learn-chunk (make-count-order :name 'two-three :first 2 :second 3))
(learn-chunk (make-count-order :name 'three-four :first 3 :second 4))
(learn-chunk (make-count-order :name 'four-five :first 4 :second 5))
(learn-chunk (make-count-order :name 'five-six :first 5 :second 6))
(learn-chunk (make-count-order :name 'six-seven :first 6 :second 7))
(learn-chunk (make-count-order :name 'seven-eight :first 7 :second 8))
(learn-chunk (make-count-order :name 'eight-nine :first 8 :second 9))
(learn-chunk (make-count-order :name 'nine-ten :first 9 :second 10))

;;;; Defining procedural rule


(defrule add-by-counting (start addend)
  "Count from ARG1 to ARG1.
ARG1 is the starting point and ARG2 is the ending point.
Each increment is 1 unit."
  (let ((current start))
    (unfold-fingers addend)
    (loop while (finger-left) do
      (let ((p (retrieve-chunk (list :chunk-type 'count-order 
				     :first current))))
	(when p
	  (setq current (count-order-second p))
	  (fold-one-finger))))
    current))


(defvar *fingers*)
(defrule unfold-fingers (n)
  (setq *fingers* n))

(defrule fold-one-finger ()
  (decf *fingers*))

(defrule finger-left ()
  (> *fingers* 0))

;; example:

(stop-actup-time (add-by-counting 2 6))