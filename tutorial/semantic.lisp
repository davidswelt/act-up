;;; Filename: semantic.lisp

;;; Author: Jasmeet Ajmani

(require "act-up" "../act-up.lisp")
(use-package :act-up)

(setf *lf* 0.5)
(setf *rt* -1)

;;;; Defining chunk type
(define-chunk-type property object attribute value)

;;;; Committing chunks to memory

(defun init-model ()
  (reset-model)
  (learn-chunk (make-property :object 'shark :attribute 'dangerous :value 'true))
  (learn-chunk (make-property :object 'shark :attribute 'locomotion :value 'swimming))
  (learn-chunk (make-property :object 'shark :attribute 'category :value 'fish))
  (learn-chunk (make-property :object 'salmon :attribute 'edible :value 'true))
  (learn-chunk (make-property :object 'salmon :attribute 'locomotion :value 'swimming))
  (learn-chunk (make-property :object 'salmon :attribute 'category :value 'fish))
  (learn-chunk (make-property :object 'fish :attribute 'breathe :value 'gills))
  (learn-chunk (make-property :object 'fish :attribute 'locomotion :value 'swimming))
  (learn-chunk (make-property :object 'fish :attribute 'category :value 'animal))
  (learn-chunk (make-property :object 'animal :attribute 'moves :value 'true))
  (learn-chunk (make-property :object 'animal :attribute 'skin :value 'true))
  (learn-chunk (make-property :object 'canary :attribute 'sings :value 'true))
  (learn-chunk (make-property :object 'canary :attribute 'color :value 'yellow))
  (learn-chunk (make-property :object 'canary :attribute 'category :value 'bird))
  (learn-chunk (make-property :object 'ostrich :attribute 'flies :value 'false))
  (learn-chunk (make-property :object 'ostrich :attribute 'height :value 'tall))
  (learn-chunk (make-property :object 'ostrich :attribute 'category :value 'bird))
  (learn-chunk (make-property :object 'bird :attribute 'wings :value 'true))
  (learn-chunk (make-property :object 'bird :attribute 'locomotion :value 'flying))
  (learn-chunk (make-property :object 'bird :attribute 'category :value 'animal)))

;;;; Defining procedural rules

(defrule semantic (arg1 arg2)
  (let ((p (retrieve-chunk (list :chunk-type 'property 
				 :object arg1 
				 :attribute 'category))))
    (if (not p)
	(print nil)
      (if (eq (property-value p) arg2)
	  (print 'yes)
	(let ((q (retrieve-chunk (list :chunk-type 'property 
				       :object (property-value p) 
				       :attribute 'category))))
	  (if (eq (property-value q) arg2)
	      (print 'yes)
	    (print 'no)))))))

(init-model)
