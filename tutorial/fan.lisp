;; -*-Mode: Act-up; fill-column: 75; comment-column: 50; -*-
;;; Filename: fan.lisp

;; To use: (unit-test)

;;; Author: Jasmeet Ajmani & David Reitter
;;; Acknowledgements: Dan Bothell



;; we use a more complex notation to find the ACT-UP file
;; relative to the location of the tutorial file.
(load (concatenate 'string (directory-namestring *load-truename*) "../load-act-up.lisp"))
(load (concatenate 'string (directory-namestring *load-truename*) "../util/actr-stats.lisp"))

;; ACT-R Parameters:
(setq *ans* nil
      *lf* .63
      *mas* 1.6
      *associative-learning* nil)


;; Model parameters:
(defparameter *model-time-parameter* 0.735)
(defparameter *model-initial-delay* 0.89)


(defvar *person-location-data* '(1.11 1.17 1.22
				      1.17 1.20 1.22
				      1.15 1.23 1.36
				      1.20 1.22 1.26
				      1.25 1.36 1.29
				      1.26 1.47 1.47))

(defvar *response*)
(defvar *response-time* nil)

;;;; chunk-types
(define-chunk-type comprehendfact relation arg1 arg2)
(define-chunk-type person)
(define-chunk-type location place)

(defun init-model ()
  (reset-model)

;; add the simple chunks first
;; without adding those chunks, the system will
;; implicitly generate them when learning the
;; `comprehendfact' chunks.
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


;;;; committing chunks to memory
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

  (pass-time *model-initial-delay*))

;; The Model
;;;; define a production
(defproc check-factual-sentence (person location target term)
  (let ((cfd
	 (retrieve-chunk
	  ;; hard constraints:
	  (append (list :chunk-type 'comprehendfact)
		  (if (eq term 'person)
		      (list :arg1 person)
		      (list :arg2 location)))
	  :cues (list person location))))
    (pass-time *model-time-parameter*)
    (if (and cfd ; if not retrieved, answer would be NO
	     (equal person (comprehendfact-arg1 cfd))
	     (equal location (comprehendfact-arg2 cfd)))
	;; answer "K" (yes)
	(if target t nil) ; YES
	;; answer "d" (no)
	(if target nil t))))




;; (list (check-factual-sentence 'earl 'castle t 'person)
;;       (check-factual-sentence 'earl 'castle t 'location)
;;       (check-factual-sentence 'captain 'bank nil 'person)
;;       (check-factual-sentence 'captain 'bank nil 'person))


;;; Environment, etc.


(defun run-model (person location target term)
  "Initializes, then runs the model for a given fact."
  (init-model)
  (let* ((result))
    (list
     ;; duration
     (stop-actup-time
       (setq result
	     (check-factual-sentence person location target term)))
     ;; result of model run
     result)))


(defun do-person-location (term)
  "Queries the model given a test set of facts."
  (let ((test-set '((lawyer store t) (captain cave t) (hippie church t)
		    (debutante bank t) (earl castle t) (hippie bank t)
		    (fireman park t) (captain park t) (hippie park t)
		    (fireman store nil) (captain store nil) (giant store nil)
		    (fireman bank nil) (captain bank nil) (giant bank nil)
		    (lawyer park nil) (earl park nil) (giant park nil)))
        (results nil))

    (dolist (sentence test-set)
      (push (list sentence 
			(apply #'run-model (append sentence (list term))))
            results))
    (mapcar #'second (sort results #'< :key #'(lambda (x) (position (car x) test-set))))))

(defun output-person-location (data)
  "Outputs and returns correlation and mean deviation of DATA"
  (let ((rts (mapcar 'first data)))
    (let ((correlation (correlation rts *person-location-data*))
	  (meandev (mean-deviation rts *person-location-data*)))
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
	  (format t "~{~8,3F (~3s)~}" (nth (+ j (* (+ i 3) 3)) data))))
      (list correlation meandev))))

(defun average-person-location ()
  "Calculates and outputs average correlations and mean deviations.
Runs model, for retrieval via person and via location and averages
over the results."
  (output-person-location 
   (mapcar #'(lambda (x y) 
	       (list (/ (+ (car x) (car y)) 2.0)  ; first element of list: time
		     (and (cadr x) (cadr y))))  ; second element: answer
	   (do-person-location 'person)
	   (do-person-location 'location))))

(defun unit-test ()
  (average-person-location))


(defun optimize-parameters ()
  (with-open-file (output "fan-results.txt" :direction :output
			  :if-exists :supersede)
    (format output "initial.delay~ttime.factor~tcorrelation~tdeviation~%")
    (let ((minimum 0.4)
	  (maximum 1.4)
	  (best-cor 0.0)
	  (best-cor-par nil)
	  (best-meandev 1.0)
	  (best-meandev-par nil)
	  ;; do not print extra output:
	  (*actr-stats-show-results* nil))
    (loop with *actr-stats-show-results* = nil
       for *model-initial-delay* from minimum to maximum by 0.005 do
	 (loop for *model-time-parameter* from minimum to maximum by 0.005
	    for data = (mapcar #'(lambda (x y) 
				   (list (/ (+ (car x) (car y)) 2.0) 
					 (and (cadr x) (cadr y))))
			       (do-person-location 'person) 
			       (do-person-location 'location))
	    do
	      (let* ((rts (mapcar 'first data))
		    (cor (correlation rts *person-location-data*))
		    (meandev (mean-deviation rts *person-location-data*)))
		(if (> cor best-cor)
		    (setq best-cor cor
			  best-cor-par (list *model-initial-delay* *model-time-parameter*)))
		(if (< meandev best-meandev)
		    (setq best-meandev meandev
			  best-meandev-par (list *model-initial-delay* *model-time-parameter*)))

		(format output "~,4F~t~,4F ~t~,4F~t~,4F~%" 
			*model-initial-delay* *model-time-parameter*
			cor
			meandev))))

    (format t "Best correlation: ~,4F at parameters ~a~%"
	    best-cor best-cor-par)
    (format t "Best mean deviation: ~,4F at parameters ~a~%"
	    best-meandev best-meandev-par))))

;; ;; At ACT-R default parameters:
;; CL-USER> (optimize-parameters)
;; Best correlation: 0.5045 at parameters (0.39999977 0.7999994)
;; Best mean deviation: 0.0985 at parameters (0.7999994 0.2549999)

;; ;; With the parameters from the classic model
;; CL-USER> (optimize-parameters)
;; Best correlation: 0.8329 at parameters (0.88999957 1.3999991)
;; Best mean deviation: 0.0565 at parameters (0.8649996 0.7349997)

 ;; R code to plot the results:
 ;; d <- read.table("fan-results.txt", header=T)
 ;; library (Hmisc)  # may need to be installed!
 ;; xYplot(correlation+deviation~model.time.parameter, data=d, method=smean.cl.boot)
 ;; xYplot(correlation+deviation~model.initial.delay, data=d, method=smean.cl.boot)
