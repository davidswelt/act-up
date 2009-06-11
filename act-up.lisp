;; ACT-UP

;; DR, 04/2009

;; NOT FULLY FUNCTIONAL       


;; production rules?



(declaim (optimize (speed 1) (space 0) (debug 3)))


(load "actr6-compatibility.lisp")  ; hack
(load "actr-aux.lisp") 

;; all chunks inherit from this structure:


(defstruct chunk

  ;; internal ACT-UP structures
  (total-presentations 0 :type integer)
  (first-presentation nil)
  (recent-presentations nil :type list) ; with the most recent one in car!
  (presentations nil :type list)
  (last-bl-activation 0)
  (activation-time nil)

  ;; we guarantee that the noise is constant
  ;; if time is constant
  (last-noise nil)
  (last-noise-time nil)
 
  (id (gensym "chunk") :type atom)
  (related-chunks nil :type list)

  ;; working on this:
  (references nil :type list)  ; list of chunks that this chunk references
  (referenced-by nil :type list) ; list of chunks that reference this chunk

  (fan nil) ; internal)
)



(defstruct meta-process
  (actUP-time 0.0 :type number)
  name
)

(defparameter *current-actUP-meta-process* (make-meta-process))



(defun update-chunk-references (chunk)
  (loop for (slot . value) in (structure-alist chunk) do
       ;; to do
       (print "not implemented")
       ))


;; parameters

(defparameter *bll* 0.5)
(defparameter *blc* 1.7) ; 1.7  ;; seems to result in a ceiling
(defparameter *rt* 1.0)  ; can be (cons 'pres 4)

(defparameter *ans* 0.2) ;; transient noise  0.2


;; a model

;; (defvar *actUP-model-parameters* '(bll blc ol dm-noise))
;; (defstruct model-parameters
;;   (bll 0.5 :type number)
;;   (blc 0.0 :type number)
;;   (ol 2 :type integer)
;;   (dm-noise 0.1 :type number)
;; )


(defstruct declarative-memory
  (chunks nil :type list))

(defstruct model 
  (parms nil :type list) 
  ;; overriding model-specific parameters. association list
  ;; of form (PARM . VALUE).
  ;; if an entry for PARM is present, it will be used rather
  ;; than the global binding.
  ;; NOT IMPLEMENTED YET.

  (dm (make-declarative-memory) :type declarative-memory))


(defparameter *current-actUP-model* (make-model))
(defparameter *actUP-time* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIENT FUNCTIONS


(defun current-actUP-model ()
  (or *current-actUP-model*
      (error "No model active.")))

(defun actUP-time ()
  (meta-process-actUP-time *current-actUP-meta-process*))

(defun actUP-pass-time (seconds)
  (setf (meta-process-actUP-time *current-actUP-meta-process*)
	(+ (meta-process-actUP-time *current-actUP-meta-process*) seconds)))


(defmacro model-chunks (model)
  `(declarative-memory-chunks (model-dm ,model)))


(defun show-chunks (model &optional constraints)
      (print (mapcar 'chunk-name (if constraints
					(search-for-chunks model constraints)
					(dm-chunks (model-dm model))))))


(defun chunk-name (chunk)
  "Chunk")

(defun explain-activation (chunk &optional cues retr-spec)
  (when chunk
    (format nil "  ~a  ~a base-level: ~a  (~a pres) pm: ~a ~a"
	    (actUP-time)
	    (chunk-name chunk)
	    (chunk-get-base-level-activation chunk)
	    (chunk-total-presentations chunk) ;; (chunk-recent-presentations chunk)
	    (if cues
		(format nil "  spreading: ~a" (chunk-get-spreading-activation chunk cues))
		"")
	    (if retr-spec 
		(format nil "partial match: ~a " (chunk-get-partial-match-score chunk retr-spec))
		"-")
	    )))
  

(defmacro normalize-slotname (slot)
  `(intern (string-upcase (symbol-name ,slot))))

(defun search-for-chunks (model args)
;  (say "searching for chunks ~a" args)
  (filter-chunks (model-chunks model) args))

(defun filter-chunks (chunk-set args)
  "Filter chunks according to ARGS.
ARGS is a list of the form (:slot1 value1 :slot2 value2),
or (slot1 value1 slot2 value2).
CHUNK-SET is the list of chunks to be filtered (1), or an associative array (2)
of the form ((X . chunk1) (Y . chunk2) ...).
returns a list of chunks in case (1) and a list of conses in case (2)."

  (loop for chunk-or-cons in chunk-set append
       (let ((c (if (consp chunk-or-cons) (cdr chunk-or-cons) chunk-or-cons)))
	 (if (loop for argval on args by #'cddr finally (return t) do
		  (let* ((slot (first argval))
			 (value (second argval))
			 (slot-value (slot-value c (normalize-slotname slot))))
		    (unless (or (equal slot-value value)
				(and (eq value 'non-nil) slot-value))
		      (return nil))))
	     (list chunk-or-cons)
	     nil))))



(defparameter *actup--chunk-slots* (mapcar #'car (structure-alist (make-chunk)))
  "Internal to ACT-UP.")


(defun learn-chunk (chunk)
"Learn chunk CHUNK.

This will note a presentation of an existing chunk in the model's DM, if
the existing chunk is unifiable with CHUNK.  If no such chunk exists in DM,
the CHUNK will be added.  If more than one such chunk exists, one of the existing
chunks is noted as 'presented'.

CHUNK may be altered by side-effect.

Returns the added chunk."
  (let* ((model *current-actUP-model*)
	 (chunk-descr
	  (loop for (slot . val) in (structure-alist chunk) append
	       (unless (member slot *actup--chunk-slots*)
		 (list slot val))))

	 ;; we're either taking an existing chunk, or we're adding a new one.
	 (chunk (or (car (search-for-chunks model chunk-descr))
		    (progn
		      (setf (chunk-total-presentations chunk) 0)
		      (setf (chunk-first-presentation chunk) (actUP-time))
		      (push chunk (model-chunks model))
		      chunk))))

    (incf (chunk-total-presentations chunk))
    (push (actUP-time) (chunk-presentations chunk))
    
    (push (actUP-time) (chunk-recent-presentations chunk))
    (if (> (length (chunk-recent-presentations chunk)) 3)
	(setf (nthcdr 3 (chunk-recent-presentations chunk)) nil)) ;; only OL 3
    (actup-pass-time 0.05) ;; 50ms
    chunk))


(defparameter *lf* 1.0)
(defparameter *le* 1.0)

(defun best-chunk (confusion-set cues &optional request-spec &rest options)
"Retrieves the best chunk in confusion set.
CONFUSION-SET is a list of chunks, out of which the chunk is returned.
CUES is a list of cues that spread activation.
OPTIONS: do not use (yet).

Simulates timing behavior."

  (setq  *last-retrieved-activation* nil)
 ;; retrieve using spreading activation from cues to confusion-set
  (if confusion-set
      (let ((best  (loop for c in confusion-set with bc = nil with bs = nil 
			when (if (eq options 'inhibit-cues) (not (member c cues)) t)
			when c ;; allow nil chunks
		  do
		    (let ((s (chunk-get-activation c cues request-spec)))
		     
		      (if (or (not *rt*) 
			      (if (consp *rt*)
				  (> (length (chunk-presentations c)) (cdr *rt*))
				  (> s *rt*)))
			  (if (or (not bc) (> s bs)) (setq bc c bs s))

		;	  (say "chunk ~a falls below RT" (concept-name c))
			  ))
		  finally
			(progn (setq *last-retrieved-activation* 
				     bs)
			       (return bc))
		    )))

	;; timing
	(if *last-retrieved-activation*
	    (actUP-pass-time (* *lf* (exp (- (* *le* *last-retrieved-activation*))))))
	
	;; (say "found best chunk: ~a " (if best (concept-name best) nil))
	best)))

(defun best-n-chunks (n confusion-set cues)
"Retrieves the best chunks in confusion set.
CONFUSION-SET is a list of chunks, out of which the best N chunks will be returned.
CUES is a list of cues that spread activation."

  (setq  *last-retrieved-activation* nil)
 ;; retrieve using spreading activation from cues to confusion-set
  (if confusion-set
      (let ((all  (loop for c in confusion-set 
		  append
		    (let ((s (+ (chunk-get-base-level-activation c)
				(if *ans* (act-r-noise *ans*) 0)
				(chunk-get-spreading-activation c cues))))
		     
		      (if (or (not *rt*) 
			      (if (consp *rt*)
				  (> (length (chunk-presentations c)) (cdr *rt*))
				  (> s *rt*)))
			  (list (cons s c)))))
			  ))
	(mapcar 'cdr (subseq (stable-sort all #'> :key #'car) 0 (min n (length all) ))))))

 
(defun blend (chunks &optional cues chunk-type retrieval-spec)
  "Return a blended variant of chunks.
Activation is calculated using spreading activation from CUES.
The returned chunk is of type CHUNK-TYPE; all CHUNKS must be
of type CHUNK-TYPE or of a supertype thereof.  If CHUNK-TYPE is not
given, all CHUNKS must be of the same class and the returned type
will be this class.
RETRIEVAL-SPEC should contain the retrieval filter used to
obtain CHUNKS; attribute-value pairs in it will be included in
the returned chunk as-is and not be blended from the CHUNKS."  

 
  (if (and chunk-type (symbolp chunk-type))
      (setq chunk-type (find-class chunk-type)))

  ;;convert flat list into assoc list
  (let ((auto-chunk-type nil) 
	(empty-chunk (make-chunk))
	(blend-activation 0)
	(retrieval-spec-alist
	 (loop for (a b) on retrieval-spec by #'cddr collect
	      (cons a b)))
	(slot-values-by-name))
    ;; collect all slots in all chunks along with their values
    (loop for c in chunks do 

	 (if auto-chunk-type
	     (unless (eq chunk-type (class-of c))
	       (error (format "blend: Found chunk of different types ~a and ~a, and no CHUNK-TYPE given."
			      nil (class-name (class-of c)) (class-name chunk-type))))
	     (if chunk-type 
		 (unless (subtypep chunk-type (class-of c))
		   (error (format "blend: Chunk of type ~a is not supertype of given type ~a."
				  nil (class-name (class-of c)) (class-name chunk-type))))
		 (setq auto-chunk-type t
		       chunk-type (class-of c))))

	 (let ((act (chunk-get-activation c)))
	   (setq blend-activation (+ blend-activation
				     (exp act)))
	   (loop for s in (structure-alist c) do
		(when (and (not (assoc (car s) retrieval-spec-alist))
			   ;; not an internal slot from "chunk"
			   (not (slot-exists-p empty-chunk (car s))))
		  (if (not (assoc (car s) slot-values-by-name))
		      (push (cons (car s) nil) slot-values-by-name))
		  (push
		   (cons act (cdr s)) ;; value
		   (cdr (assoc (car s) slot-values-by-name)))))))
    
    (setq blend-activation (if (> blend-activation 0) (log blend-activation) 0)) 
   
    ;; let's blend
    ;; currently, only numerical values are supported    
    
    
    ;; to do: return real object
    ;; (class-of c) ??
    ;; (make-instance class ...)

   (if (and (> blend-activation *rt*) (or retrieval-spec slot-values-by-name))
       ;; (apply #'make-instance    does not work for structures (openmcl)
       ;; 	      chunk-type 
       (apply (find-symbol (format nil "MAKE-~a" (class-name chunk-type)))
	      :last-bl-activation blend-activation
	      :activation-time (actup-time)
	      (append
	       retrieval-spec
	       (loop for sv in slot-values-by-name append
		  ;; calculate weighted mean
		  ;; weights are determined by boltzmann activation of each chunk
		  ;; i.e. retrieval probability
		  ;; Pi = exp(Ai/t) / sum(j, exp(Aj/t))
		    
		    (let* ((boltzmann-total 0)
			   (sum
			    (loop for (a . n) in (cdr sv) sum
				 (if (numberp n)
				     (let ((boltzmann-act  (exp (/ a *blend-temperature*))))
				       (setq boltzmann-total (+ boltzmann-total boltzmann-act)) ; denominator in boltzmann eq
				       (* boltzmann-act n))
				     (return nil)))))
		      (if sum
			  (list (car sv) (/ sum boltzmann-total)))))))
       nil)))

(defun reset-mp ()
  (setq *current-actUP-meta-process* (make-meta-process)))

(defun reset-model ()
  (setq *current-actUP-model* (make-model)))

(defun reset-sji-fct (chunk)
  (setf (chunk-related-chunks chunk) nil))

(defun add-sji-fct (list)
  (loop for (c1 c2 s) in list do
       (unless (eq c1 c2)
	 (setf (chunk-related-chunks c1) (delete (rassoc c2 (chunk-related-chunks c1)) (chunk-related-chunks c1)))
	 (setf (chunk-related-chunks c1)
	       (insert-by-weight (list (cons s c2)) (chunk-related-chunks c1))))
       ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERNAL

; private 
;; (defun chunk-slot (chunk slot-name)
;;   (let ((sym (find-symbol (format nil "~A-~A" (type-of chunk) slot-name)
;;                              ;; #.(package-name *package*
;; 			     )))
;;     (if sym
;; 	(funcall (symbol-function
;; 		  sym)
;; 		 chunk)
;; 	(error (format nil "Slot ~a not found." slot-name)))))

(defun chunk-slot (chunk slot-name)
 (slot-value chunk slot-name))
 

(defun chunk-get-fan  (j i chunk-set)

  ;; how to iterate over all slots?

  (let* ((j-al (structure-alist j))
	 (j-len (length j-al)))
    
    (loop for c in chunk-set sum
	 (structure-value-count c j (chunk-id j))
	 ;; to do
)))


(defun chunk-get-noise (chunk)
  (if *ans* 
      (or (and (eq (actUP-time) (chunk-last-noise-time chunk))
	       (chunk-last-noise chunk))
	  (progn
	    (setf (chunk-last-noise chunk) (act-r-noise *ans*)
		  (chunk-last-noise-time chunk) (actUP-time))
	    (chunk-last-noise chunk)))
      0))

(defun chunk-get-activation (chunk &optional cue-chunks retrieval-spec)
  "Calculate current activation of chunk"

  (let ((base-level (chunk-get-base-level-activation chunk))
	(spreading (chunk-get-spreading-activation chunk cue-chunks))
	(partial-matching (chunk-get-partial-match-score chunk retrieval-spec))
	(noise (chunk-get-noise chunk)))

	(+ base-level spreading partial-matching noise)))


;; export

    

; to do
; relax "member" - can't work like this
; update activation
; update references
; initialize name if necessary
; 


   ;; set time


;   (setf (declarative-memory-chunks (model-dm (current-actUP-model))) chunks)






(defun chunk-get-base-level-activation (chunk)

  ;; we're using the Optimized Learning function

  (let ((d *bll*)) ;; (model-parameters-bll (model-parms (current-actUP-model)))
    (+
     ;; standard procedure
     (loop for pres in (chunk-recent-presentations chunk) sum
	  (expt (max 1 (- (actUP-time) pres)) (- d)))
     
     ;; initial BL activation  (e.g., from blending)
     (or (chunk-last-bl-activation chunk) 0)

     ;; optimized learning
     (let ((k (length (chunk-recent-presentations chunk))))
       (if (and (> (chunk-total-presentations chunk) k) (chunk-first-presentation chunk))
	   (let ((last-pres-time (max 1 (- (actUP-time) (or (car (last (chunk-recent-presentations chunk))) 
							       (chunk-first-presentation chunk))))) ;; 0? ;; tn
		 (first-pres-time (max 1 (- (actUP-time) (chunk-first-presentation chunk)))))
	     (if (and first-pres-time
		      (not (= first-pres-time last-pres-time)))
		 (progn
		   (/ (* (- (chunk-total-presentations chunk) k) 
			 (max 0.1 (- (expt first-pres-time (- 1 d)) (expt last-pres-time (- 1 d)))))
		      (* (- 1 d) (max 0.1 (- first-pres-time last-pres-time)))))
		 0))
	   0))
     *blc*)))

(defun chunk-get-spreading-activation (chunk cues)
  (if cues
      (* 1
      (/ (loop for cue in cues sum
	      (or (car (rassoc chunk (chunk-related-chunks cue))) 0.0))
	 (length cues)))
      0))



(defparameter *pm* 1.0)
(defun chunk-get-partial-match-score (chunk retrieval-spec)
  (if *pm*
      (progn
	(* *pm*
	   (loop for (s v) on retrieval-spec  by #'cddr sum
		(value-get-similarity (slot-value chunk (normalize-slotname s)) v))
	   ))

      ;; else
      0))

(defun value-get-similarity (v1 v2)
  (if (and (numberp v1) (numberp v2))
      (if (= v1 v2)
	  0
	  (- (abs (- v1 v2))))  ;; could be done better!
      0 ;; not implemented yet
    ))


;; experimental

;; maybe we'll leave all parameters global for now
;; (defun switch-to-model (model)

;;   ;; handle dynamic, global variables as model-parameters
;;   (loop for p in *actUP-model-parameters* do
;;        (let ((sym (format nil "*~a*" p)))


;;        (if (boundp sym) ;; dynamically bound
;; 	   (setf (slot-value 
;; 		  (model-parameters *current-actUP-model*)
;; 		  p)
;; 		 (symbol-value sym))
;; 	  ;; else
;; 	   (defvar (symbol-value sym) nil)) ;; make sure it's dynamic and defined
	 
;;        (setf sym (slot-value (model-parameters model) p))))
;;   (setq *current-actUP-model* model))


(provide "act-up")