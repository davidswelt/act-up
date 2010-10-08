;;; Filename: tuto5pm.lisp

;; To use: (unit-test)

;;; Author: Jasmeet Ajmani
;;; Acknowledgements: Dan Bothell


(load "../actr-stats")
(require "act-up" "../act-up.lisp")
(use-package :act-up)


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
(defparameter *correlation-pm* nil)
(defparamater *meandev-pm* nil)

;;;; chunk-types
(define-chunk-type comprehendfact relation arg1 arg2)
(define-chunk-type person)
(define-chunk-type location place)

(defun init-model ()
  (reset-model)
;;;; committing chunks to memory
  (learn-chunk (make-comprehendfact :relation 'in :arg1 'hippie :arg2 'park))
  (learn-chunk (make-comprehendfact :relation 'in :arg1 'hippie :arg2 'church))
  (learn-chunk (make-comprehendfact :relation 'in :arg1 'hippie :arg2 'bank))
  (learn-chunk (make-comprehendfact :relation 'in :arg1 'captain :arg2 'park))
  (learn-chunk (make-comprehendfact :relation 'in :arg1 'captain :arg2 'cave))
  (learn-chunk (make-comprehendfact :name 'deb-bank :relation 'in :arg1 'debutante :arg2 'bank))
  (learn-chunk (make-comprehendfact :relation 'in :arg1 'fireman :arg2 'park))
  (learn-chunk (make-comprehendfact :relation 'in :arg1 'giant :arg2 'beach))
  (learn-chunk (make-comprehendfact :relation 'in :arg1 'giant :arg2 'castle))
  (learn-chunk (make-comprehendfact :relation 'in :arg1 'giant :arg2 'dungeon))
  (learn-chunk (make-comprehendfact :relation 'in :arg1 'earl :arg2 'castle))
  (learn-chunk (make-comprehendfact :relation 'in :arg1 'earl :arg2 'forest))
  (learn-chunk (make-comprehendfact :relation 'in :arg1 'lawyer :arg2 'store))

  (learn-chunk (make-person :name 'hippie))
  (learn-chunk (make-person :name 'captain))
  (learn-chunk (make-person :name 'debutante))
  (learn-chunk (make-person :name 'fireman))
  (learn-chunk (make-person :name 'giant))
  (learn-chunk (make-person :name 'earl))
  (learn-chunk (make-person :name 'lawyer))

  (learn-chunk (make-location :name 'park))
  (learn-chunk (make-location :name 'church))
  (learn-chunk (make-location :name 'bank))
  (learn-chunk (make-location :name 'cave))
  (learn-chunk (make-location :name 'beach))
  (learn-chunk (make-location :name 'castle))
  (learn-chunk (make-location :name 'dungeon))
  (learn-chunk (make-location :name 'forest))
  (learn-chunk (make-location :name 'store))

  (add-sji-fct 
   (list 
    (list 'lawyer 'store 0.90685)
    (list 'earl 'forest 1.40824)
    (list 'earl 'castle 1.00277)
    (list 'giant 'dungeon 1.12055)
    (list 'giant 'castle 0.71509)
    (list 'giant 'beach 1.12055)
    (list 'fireman 'park 1.12055)
    (list 'debutante 'bank 1.40824)
    (list 'captain 'cave 1.40824)
    (list 'captain 'park 0.71509)
    (list 'hippie 'bank 0.71509)
    (list 'hippie 'church 1.12055)
    (list 'hippie 'park 0.42741)
    ))
  )


(list (test-sentence-model 'earl 'castle t 'person)
      (test-sentence-model 'earl 'castle t 'location)
      (test-sentence-model 'captain 'bank nil 'person)
      (test-sentence-model 'captain 'bank nil 'person))

(defun unit-test()
  (average-person-location-pm))


(defun run-model (person location target term)
  (init-model)
  (let ((start-time (actup-time)))
    (reverse (list (test-sentence-model-pm person location target term)
		   (- (actup-time) start-time)))))


(defun do-person-location-pm (term) 
  (let ((test-set '(('lawyer 'store t)('captain 'cave t)('hippie 'church t)
                      ('debutante 'bank t)('earl 'castle t)('hippie 'bank t)
                      ('fireman 'park t)('captain 'park t)('hippie 'park t)
                      ('fireman 'store nil)('captain 'store nil)('giant 'store nil)
                      ('fireman 'bank nil)('captain 'bank nil)('giant 'bank nil)
                      ('lawyer 'park nil)('earl 'park nil)('giant 'park nil)))
        (results nil))

    (dolist (sentence test-set)
      (push (list sentence 
			(apply #'run-model (append sentence (list term))))
            results))
    (mapcar #'second (sort results #'< :key #'(lambda (x) (position (car x) test-set))))))

(defun average-person-location-pm ()
  (output-person-location-pm 
   (mapcar #'(lambda (x y) 
	       (list (/ (+ (car x) (car y)) 2.0)  ; first element of list: time
		     (and (cadr x) (cadr y))))  ; second element: answer
	   (do-person-location-pm 'person) 
	   (do-person-location-pm 'location)))
  (list *correlation-pm* *meandev-pm*))

(defun output-person-location-pm (data)
  (let ((rts (mapcar 'first data)))
    (setq *correlation-pm* (correlation rts *person-location-data*))
    (setq *meandev-pm* (mean-deviation rts *person-location-data*))
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

;;;; defining productions
(defproc test-sentence-model-pm (person location target term)
  (let* (;(*debug* *all*)
	 (cfd (debug-detail
	       (retrieve-chunk
		;; hard constraints:
		(append (list :chunk-type 'comprehendfact)
			(if (eq term 'person)
			    (list :arg1 person)
			  (list :arg2 location)))
		;; cues:
		(list person location)
	       ))))
    (print cfd)
    (if (and cfd ; if not retrieved, answer would be NO
	     (equal person (comprehendfact-arg1 cfd))
	     (equal location (comprehendfact-arg2 cfd)))
	;; answer "K" (yes)
	(if target t nil) ; YES
	;; answer "d" (no)
	(if target nil t))))