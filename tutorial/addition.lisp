;;; Filename: addition.lisp

;;; to use: (add-digit 1 2 4 1)

;;; Author: David Reitter/ Jasmeet Ajmani

;; we use a more complex notation to find the ACT-UP file
;; relative to the location of the tutorial file.
(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))

(setq *rt* -5) ; ensure retrieval

;;;; Defining chunk-types

(define-chunk-type additionfact addend1 addend2 sum)
(define-chunk-type carryfact total remainder quotient)

;;;; Defining chunks here and committing chunks to memory

(loop for a from 0 to 18 do
      (loop for b from 0 to 9 do
	    (learn-chunk (make-additionfact
			  :addend1 a
			  :addend2 b
			  :sum (+ a b)))))

(loop for s from 10 to 18 do
      (learn-chunk (make-carryfact 
		    :total s
		    :remainder (rem s 10)
		    :quotient (truncate (/ s 10)))))

;;;; Defining procedural rule

(defproc add-digit (ten1 one1 ten2 one2)
  "Add two digits."
  (let ((sumones (additionfact-sum (retrieve-chunk (list :chunk-type 'additionfact
							 :addend1 one1 
							 :addend2 one2))))
	(sumtens (additionfact-sum (retrieve-chunk (list :chunk-type 'additionfact
							 :addend1 ten1 
							 :addend2 ten2)))))
    (when (> sumones 9)
      (let ((cf (retrieve-chunk (list :chunk-type 'carryfact 
				      :total sumones))))
	(setq sumones (carryfact-remainder cf))
	(setq sumtens (additionfact-sum 
		       (retrieve-chunk (list :chunk-type 'additionfact
					     :addend1 sumtens
					     :addend2 (carryfact-quotient cf)))))))
    (list sumtens sumones)))