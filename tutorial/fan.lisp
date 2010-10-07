;;; Filename: fan.lisp

;;; Author: David Reitter
;;; Acknowledgements: Dan Bothell

; to run: (average-person-location)

;; (load "act-up.lisp")

(require "act-up" "../act-up.lisp")
(use-package :act-up)
(load "../actr-stats.lisp")


;;(sgp :v t :act nil :esc t :lf .63 :mas 1.6 :ga 1.0 :imaginal-activation 1.0) 

(setq *ans* nil
      *lf* .63
      *mas* 1.6
      *associative-learning* nil)

(defvar *person-location-data* '(1.11 1.17 1.22
				 1.17 1.20 1.22
				 1.15 1.23 1.36
				 1.20 1.22 1.26
				 1.25 1.36 1.29
				 1.26 1.47 1.47))

(defvar *response*)
(defvar *response-time* nil)

(defparameter  *model-time-parameter* 0.34) 

;;;; Defining chunk types
(define-chunk-type comprehendfact relation arg1 arg2)
(define-chunk-type person)
(define-chunk-type location place)

;;;; committing chunks to memory

(defun init-model ()
  (reset-model)
  (learn-chunk (make-comprehendfact :name 'hippie-in-park :relation 'in :arg1 'hippie :arg2 'park))
  (learn-chunk (make-comprehendfact :name 'hippie-in-church :relation 'in :arg1 'hippie :arg2 'church))
  (learn-chunk (make-comprehendfact :name 'hippie-in-bank :relation 'in :arg1 'hippie :arg2 'bank))
  (learn-chunk (make-comprehendfact :name 'captain-in-park :relation 'in :arg1 'captain :arg2 'park))
  (learn-chunk (make-comprehendfact :name 'captain-in-cave :relation 'in :arg1 'captain :arg2 'cave))
  (learn-chunk (make-comprehendfact :name 'debutante-in-bank :relation 'in :arg1 'debutante :arg2 'bank))
  (learn-chunk (make-comprehendfact :name 'fireman-in-park :relation 'in :arg1 'fireman :arg2 'park))
  (learn-chunk (make-comprehendfact :name 'giant-in-beach :relation 'in :arg1 'giant :arg2 'beach))
  (learn-chunk (make-comprehendfact :name 'giant-in-castle :relation 'in :arg1 'giant :arg2 'castle))
  (learn-chunk (make-comprehendfact :name 'giant-in-dungeon :relation 'in :arg1 'giant :arg2 'dungeon))
  (learn-chunk (make-comprehendfact :name 'earl-in-castle :relation 'in :arg1 'earl :arg2 'castle))
  (learn-chunk (make-comprehendfact :name 'earl-in-forest :relation 'in :arg1 'earl :arg2 'forest))
  (learn-chunk (make-comprehendfact :name 'lawyer-in-store :relation 'in :arg1 'lawyer :arg2 'store))
  )

;;;; Defining Procedural Rules

(defproc test-sentence-model (person location target term)
  (let* ((cfd ; (debug-detail
	       (retrieve-chunk
		;; hard constraints:
		(append (list :chunk-type 'comprehendfact)
			(if (eq term 'person)
			    (list :arg1 person)
			  (list :arg2 location)))
		;; cues:
		(list person location)
	       )))
    (if (not cfd)
	(print "could not retrieve comprehendfact!"))
    (pass-time *model-time-parameter*)
    ;(format t "~s ~s ~s ~s ~%" person location target term)
    
    (when cfd
	(if (equal person (comprehendfact-arg1 cfd))
	    (if (equal location (comprehendfact-arg2 cfd)) 
		(progn  '(K)) 
	      (progn '(D)))
	    (progn  '(D))))))

(defun run-model (person location target term)
  (init-model)
  (let ((start-time (actup-time)))
    (test-sentence-model person location target term)
    (- (actup-time) start-time)))


(defun do-person-location (term) 
  (let ((test-set '((lawyer store t)(captain cave t)(hippie church t)
                      (debutante bank t)(earl castle t)(hippie bank t)
                      (fireman park t)(captain park t)(hippie park t)
                      (fireman store nil)(captain store nil)(giant store nil)
                      (fireman bank nil)(captain bank nil)(giant bank nil)
                      (lawyer park nil)(earl park nil)(giant park nil)))
        (results nil))

    (dolist (sentence test-set)
      (push (list sentence  (list
			(apply #'run-model (append sentence (list term)))))
            results))
    (mapcar #'second (sort results #'< :key #'(lambda (x) (position (car x) test-set))))))

(defun average-person-location ()
  (output-person-location 
   (mapcar #'(lambda (x y) 
	       (list (/ (+ (car x) (car y)) 2.0) 
		     (and (cadr x) (cadr y))))
	   (do-person-location 'person) 
	   (do-person-location 'location))))

(defun output-person-location (data)
  (let ((rts (mapcar 'first data)))
    (correlation rts *person-location-data*)
    (mean-deviation rts *person-location-data*)
    (format t "~%TARGETS:~%                         Person fan~%")
    (format t  "  Location      1             2             3~%")
    (format t "    fan")
    
    (dotimes (i 3)
      (format t "~%     ~d    " (1+ i))
      (dotimes (j 3)
        (format t "~{~8,3F (~3s)~}" (nth (+ j (* i 3)) data))))
    
    (format t "~%~%FOILS:")
    (dotimes (i 3)
      (format t "~%     ~d    " (1+ i))
      (dotimes (j 3)
        (format t "~{~8,3F (~3s)~}" (nth (+ j (* (+ i 3) 3)) data))))))


(defun optimize-parameters ()
  (loop for *model-time-parameter* from 0.2 to 0.4 by 0.005
       for data = (mapcar #'(lambda (x y) 
	       (list (/ (+ (car x) (car y)) 2.0) 
		     (and (cadr x) (cadr y))))
	   (do-person-location 'person) 
	   (do-person-location 'location))
       do
       (let ((rts (mapcar 'first data)))
	 (format t "time: ~a~%" *model-time-parameter*)
	 (correlation rts *person-location-data*)
	 (mean-deviation rts *person-location-data*))))
       
