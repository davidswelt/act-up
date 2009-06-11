
(require "act-up" "act-up.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST


;; model definition

(when nil



(defstruct (person
	     (:include chunk))
  name
  city)
  

(setq *current-actUP-model* (make-model))
(learn-chunk (make-person :name "sherice" :city "pittsburgh"))
(learn-chunk (make-person :name "david"))

; (make-person :name "dave" :chunk-name 'd-chunk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BENCHMARK


(defstruct (concept (:include chunk)) ;; (concept (:print-object print-concept))
  name
  original
  concept
  related-concepts  ; ordered alist
  components ; set
  acquired
)

(defparameter my-random-state (make-random-state))
(defvar *constant-meaning-space* nil)
(defun reset-meaning-space (models concept-names &optional (noise 0))
  (loop for model in models do
       (setf (model-chunks model) nil))

    
    (dolist (cname  concept-names)
      
      (loop for model in models do
	   (let* ((concept (make-concept :name cname 
					 :concept nil ;; itself
					 :first-presentation -473040000 ;; -473040000   ; 15 years back
					 :recent-presentations '(-10022400 -5011200) ; 10,5 days back
					 :total-presentations  (* 30000 (random-pareto))
					 )))
	     (push concept (model-chunks model))
	     
	     ;;(print (chunk-get-base-level-activation concept))
	     (reset-sji-fct concept))))
	 
  ;; and create association network
  ;; we want all models to have the same common ground
  (let ((sims (or *constant-meaning-space*
		  (loop for cn1 in concept-names collect
		       (loop for cn2 in concept-names collect

			    (* 0.001 (random 1000 my-random-state)))))))

    (setq *constant-meaning-space* sims)

    ;; add noise

    (loop for model in models do
	 (loop
	    for concept in (noisy-shuffle (copy-list (model-chunks model)) 0)
	    for sim1 in sims do
	    (loop
	       for concept2 in (model-chunks model) 
	       for sim in sim1 do
	       (unless (eq concept concept2)
		 (let ((simn (if (eq noise 'max)
				 ;; noise==max: every model has its own
				 (* 0.001 (random 1000 my-random-state))

				 ;; mixture of the two distributions

				 (+ (* noise (* 0.001 (random 1000 my-random-state))) (* (- 1 noise) sim))

				 ;;sim ; no noise
				 ;; otherwise, just add the noise
				 ;; (+ sim (or (and (> noise 0.0) (act-r-noise noise)) 0))
				 )))
		   (add-sji-fct (list (list concept concept2 simn) 
				      (list concept2 concept simn))))) 
		 )))))

(defvar *number-of-concepts*  1000)
(reset-meaning-space (list *current-actUP-model*)
		     (loop for i from 1 to *number-of-concepts* collect 
			  (intern (string-upcase (format nil "concept~a" i)))))

(dotimes (n 100)
  (let ((cues
	 (loop for i from 1 to 3
	    collect
	      (choice (model-chunks *current-actUP-model*)))))

    (best-chunk (model-chunks *current-actUP-model*) cues)))
)
