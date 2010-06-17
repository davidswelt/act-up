(setq *print-level* 10)
;; ACT-UP
;; (C) Carnegie Mellon University,
;; (C) David Reitter, 2010

;; reitter@cmu.edu

;; to load:
;; (require "act-up" "act-up.lisp")
;; (use-package :act-up)


;; to do:
;; when retrieving a chunk from DM
;; i can modify it (it's not read-only)
;; and that would modify the original chunk in DM
;; that's not right.. it should be learned for this to happen
;; should retrieve-chunk return a copy? 

;; Solution
;; When first adding the chunk, make a copy of the chunk, where all attributes that are non-act-up internal
;; are copied and marked as read-only.  
;; if they are act-up chunks, they should probably be changed to the chunk's names.
;; we could probably also offer a "retrieve-chunk-copy" function that retrieves a read/write copy.



(declaim (optimize (speed 0) (space 0) (debug 3)))
 

(defpackage :act-up
  (:documentation "The ACT-UP library.  Defines a number of functions
and macros implementing the ACT-R theory (Anderson&Lebiere 1998,
Anderson 2007, etc.).
(C) 2010, David Reitter, Carnegie Mellon University.")
  (:use :common-lisp))

(in-package :act-up)



(load (format nil "~a/actr6-compatibility.lisp" (directory-namestring *load-truename*)))
(load (format nil "~a/actr-aux.lisp" (directory-namestring *load-truename*)))
(load (format nil "~a/act-up-util.lisp" (directory-namestring *load-truename*)))

(defstruct meta-process
  "An ACT-UP meta process.
A meta process keeps track of time for one or more models."
  (actUP-time 0.0 :type number)
  name
)
(export '(meta-process))

(defparameter *current-actUP-meta-process* (make-meta-process))

;; Debugging




(defvar *critical* 0 "Constant for `*debug*': Show only critical messages.")
(defvar *error* 5 "Constant for `*debug*': Show errors and more important messages.")
(defvar *warning* 10 "Constant for `*debug*': Show warnings and more important messages.")
(defvar *informational* 100 "Constant for `*debug*': Show informational and more important messages.")
(defvar *detailed* 300 "Constant for `*debug*': Show detailed log output .")
(defvar *all* 1000 "Constant for `*debug*': Show all messages (maximum detail).")
(export '(*critical* *warning* *informational* *detailed* *all* *debug*
	  *debug-to-log* debug-log debug-clear debug-detail debug-detail*))

(defparameter *debug* *warning*
  "Level of debug output currently in effect.
The following constants may be used:

*critical* *warning* *informational* *all*

The parameter `*debug-to-log*' is helpful in logging debug messages to a file.")

(defparameter *debug-to-log* nil
"Enable off-screen logging of debug output.
If t, ACT-UP logs all debug messages not to standard output,
but to a buffer that can be read with `debug-log' and cleared with `debug-clear'.
If a stream, ACT-UP logs to the stream.")

(defvar *debug-stream* nil)

(defun debug-log ()
  "Returns logged ACT-R output.
If `*debug-to-log*' is set to t, the ACT-UP debug log may be
retrieved using this function."
  (if *debug-stream* 
      (get-output-stream-string *debug-stream*))) 

(defun debug-clear ()
  "Clear the ACT-UP debug log buffer."
  (when *debug-stream*
      (close *debug-stream*)
      (setq *debug-stream* nil)))

(defmacro debug-print (level format &rest args)
  `(when (and *debug*
	      (<= ,level *debug*))
     (debug-print-internal ',format ,@args)))
 
(defun debug-print-internal (format &rest args)
  (if *debug-to-log*
    (if (and (not *debug-stream*) (not (streamp *debug-to-log*)))
	(setq *debug-stream* (make-string-output-stream))))
  
  (when format
      (let ((*print-circle* t) (*print-pretty* t)
	    (*print-pprint-dispatch* *print-pprint-dispatch*))
	(set-pprint-dispatch 'actup-chunk #'pc)	    

	(apply #'format (or (if (streamp *debug-to-log*) *debug-to-log* (if *debug-to-log* *debug-stream*)) t) format args)
	(set-pprint-dispatch 'actup-chunk nil)
	)))


(defmacro debug-detail (&body body)
  "Evaluates BODY while outputting ACT-UP debug information."
  `(let  ((*debug* *all*))
    ,@body))


(defmacro debug-detail* (&body body)
  "Evaluates BODY while logging ACT-UP debug information.
The log output can be retrieved with `debug-log'."
  `(let  ((*debug* *all*) (*debug-to-log* t))
     (debug-clear)
    ,@body))

(export '(show-utilities))
(defun show-utilities ()
  "Prints a list of all utilities in the current model."
  (maphash (lambda (x y) (print (cons x (rule-utility y))))
	   (act-up::procedural-memory-regular-rules (act-up::model-pm (current-model))))
  (print "Compiled rules not shown.")
  nil)

;; CHUNKS


;; all chunks inherit from this structure:


(export '(actup-chunk define-chunk-type 
	  ;; public members of actup-chunk:
	  name chunk-type))

(defstruct actup-chunk
  "Type defining an ACT-UP chunk.
Derive your own chunks using this as a base structure
by using `define-chunk'."
  ;; helpful for debugging
  (name (gensym "CHUNK") :read-only t)
  (comment) ;; documentation / comments
  (chunk-type nil)
  (attrs nil)  ;; list of user-defined slots

  ;; internal ACT-UP structures
  (total-presentations 0 :type integer)
  (first-presentation (actup-time))
  (recent-presentations nil :type list) ; with the most recent one in car!
  (presentations nil :type list)
  (last-bl-activation 0)
  (activation-time nil)

  ;; we guarantee that the noise is constant
  ;; if time is constant
  (last-noise nil)
  (last-noise-time nil)
 
  (id (gensym "actupchunk") :type atom)
  (related-chunks nil :type list)  ;; references to other chunks
  ;; this chunk may serve as cue for the chunks listed here.
  ;; assoc list with entries of form (chunk-name . <actup-link>)
  (references nil :type list) ;; other chunks referring to this one
  ;; this chunk will receive spreading activation if any of the cues listed here are in the context
  ;; assoc list with entries of form (chunk-name . <actup-link>)
  
  ;; other chunks that mention this chunk in one of their values
  (occurs-in nil :type list) 
  ;; similarities
  (similar-chunks nil :type list)
  (fan nil) ; internal)
)

(defstruct actup-link
  "Link between two chunks.
Includes Sji/Rji weights and cooccurrence data."
  (sji nil) ; if not set, rji (learning) is used.
  (rji 0.0)
  (fcn 0 :type integer))
  
(defun safe-slot-value (obj slot)
  (handler-case
      (slot-value obj slot)
    (error (_v) _v nil)))
(export '(pc))
(defun pc (stream obj) 
  "Print a human-readable representation of chunk OBJ to STREAM.
Set stream to t to output to standard output."
  (let ((obj (get-chunk-object obj))
	(*print-circle* t)
	(*print-level* 3)
	(stream (or stream t)))
    (handler-case
     (progn
       (format stream "~a~%" (actup-chunk-name obj))
       (loop for slot in (actup-chunk-attrs obj)
	  for val = (safe-slot-value obj slot)
	 
	  do
	    (format stream "~a: ~a~%"  slot (get-chunk-name val)))
       (format stream "~%")
       
       ;; (if (actup-chunk-related-chunks obj)
       ;; 	   (format stream "related chunks: ~a~%" (mapcar (lambda (x) (if (actup-chunk-p x) (actup-chunk-name x) x)) (actup-chunk-related-chunks obj))))
       (format stream "~%"))
     (error (v) (progn (format stream "ERR~a" v) nil)))))

(defun make-chunk (&rest args)
  "Define an ACT-UP chunk.
Arguments should consist of named chunk feature values: ARGS is a list
of the form (:name1 val1 :name2 val2 ...), whereas names correspond to
slot names as defined with `define-slots'.  

An attribute called `:name' should be included to specify the unique
name of the chunk (the name may not be used for any other chunk in
the model). 

If chunk types are defined with `define-chunk-type', then use the
`make-TYPE' syntax instead."

  (apply #'make-actup-chunk args))

(defun defstruct-attr-list (members)
  (loop for m in members collect
       (if (consp m)
	   (car m) m)))

(defmacro define-chunk-type (type &rest members)
  "Define a chunk type of name TYPE.
MEMBERS should contain all possible elements of the chunk type.
TYPE may be a symbol or a list of form (name2 :include parent-type),
whereas PARENT-TYPE refers to another defined chunk type whose
elements will be inherited.
MEMBERS may be a list of symbols, or also a list of member
specifiers as used with the lisp `defstruct' macro, which see.
  
Chunks make be created by invoking the make-TYPE function, whereas
TYPE stands for the name of the chunk type as defined with this
macro. An attribute called `:name' should be included to specify the
unique name of the chunk (the name may not be used for any other chunk
in the model). "
  
  (let* ((name-and-options type)
	 (incl
	  (if (consp name-and-options)
	      (list (car name-and-options)
		    (if (eq (cadr name-and-options) :include)
			`(:include ,(caddr name-and-options))
			(error "define-chunk-type: faulty options in NAME.")))
	      (list name-and-options `(:include actup-chunk
				       (chunk-type ',type)
				       (attrs ',(defstruct-attr-list members)))))))
    `(defstruct ,incl
       ,@members)
    ))
 

(defmacro define-slots (&rest slot-names)
  "Define slots to be used in chunks of this process.
Only slot names defined using this macro may be used in chunks.
Overrides any slot set defined earlier."
  `(define-chunk-type chunk ,@slot-names))


;; (macroexpand '(define-chunk-type test one two))


;; parameters

(defparameter *bll* 0.5 "Base-level learning decay parameter for declarative memory.
See also: ACT-R parameter :bll")
(defparameter *blc* 0.0 "Base-level constant parameter for declarative memory.
See also: ACT-R parameter :blc") 
(defparameter *rt* 0.0 "Retrieval Threshold parameter for declarative memory.
Chunks with activation lower than `*rt*' are not retrieved.
See also: ACT-R parameter :rt")  ; can be (cons 'pres 4)

(defparameter *ans* 0.2 "Transient noise parameter  for declarative memory.
See also: ACT-R parameter :ans") ;; transient noise  

(defparameter *dat* 0.05 "Default time that it takes to execut an ACT-UP rule in seconds.
See also: ACT-R parameter :dat  [which pertains to ACT-R productions]")
(export '(*bll* *blc* *rt* *ans* *dat*))

;; a model

;; (defvar *actUP-model-parameters* '(bll blc ol dm-noise))
;; (defstruct model-parameters
;;   (bll 0.5 :type number)
;;   (blc 0.0 :type number)
;;   (ol 2 :type integer)
;;   (dm-noise 0.1 :type number)
;; )

(defstruct declarative-memory
  (chunks nil :type list)
  (total-presentations 0 :type integer))

(defstruct procedural-memory
  (regular-rules (make-hash-table))
  (compiled-rules (make-tree))
  (rule-queue nil :type list))

(defstruct model 
  (parms nil :type list)
  ;; overriding model-specific parameters. association list
  ;; of form (PARM . VALUE).
  ;; if an entry for PARM is present, it will be used rather
  ;; than the global binding.
  ;; NOT IMPLEMENTED YET.
  (pm (make-procedural-memory) :type procedural-memory)
  (dm (make-declarative-memory) :type declarative-memory))



(defparameter *current-actUP-model* (make-model))
(defparameter *actUP-time* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIENT FUNCTIONS

(export '(current-model set-current-model make-model with-current-model 
	  actUP-time stop-actup-time
	  pass-time model-chunks 
	  defrule assign-reward
	  define-slots define-chunk-type
	  make-chunk ; for untyped chunks
	  show-chunks chunk-name explain-activation
	  retrieve-chunk blend-retrieve-chunk
	  filter-chunks learn-chunk best-chunk blend reset-mp reset-model
	  reset-sji-fct set-similarities-fct add-sji-fct set-dm-total-presentations set-base-level-fct set-base-levels-fct))


(defun current-model ()
  "Evaluates to the currently active ACT-UP model."
  (or *current-actUP-model*
      (error "No model active.")))

(defun current-actUP-model ()
  "Evaluates to the currently active ACT-UP model."
  (or *current-actUP-model*
      (error "No model active.")))

(defun set-current-model (new-model)
  "Switches the currently active ACT-UP model.
This may set a range of model parameters."
  (setq *current-actUP-model* new-model))

(defmacro with-current-model (model &body body)
  "Execute forms in BODY with the ACT-UP model MODEL being current."
  `(let ((*current-actUP-model* ,model))
     ,@body))

;; (defun make-model ()
;;   "Create an ACT-UP model.
;; The model object is returned, but not used as current ACT-UP model.
;; See also `set-current-model'."
;;   (make-model))


(defmacro stop-actup-time (&body body)
  "Returns execution time of BODY in current ACT-UP model.
Evaluates BODY.  See also `actup-time'."

  `(let* ((mp *current-actUP-meta-process*)
	  (actup---actup-time-t0 (meta-process-actUP-time mp)))
     ,@body
     (- (meta-process-actUP-time mp) actup---actup-time-t0)))


(defun actup-time (&optional meta-process)
  "Returns the current runtime.
An optional parameter META-PROCESS specifies the meta-process to use.
It defaults to the current meta-process."
  (meta-process-actUP-time (or meta-process *current-actUP-meta-process*)))

(defun pass-time (seconds &optional meta-process)
  "Simulates the passing of time.
An optional parameter META-PROCESS specifies the meta-process to use.
It defaults to the current meta-process."
  (if (> seconds 0)
      (setf (meta-process-actUP-time (or meta-process *current-actUP-meta-process*))
	    (+ (meta-process-actUP-time (or meta-process *current-actUP-meta-process*)) seconds))))


(defmacro model-chunks (model)
  "Evaluates to the list of chunks in the given model MODEL."
  `(declarative-memory-chunks (model-dm ,model)))


(defun show-chunks (model &optional constraints)
  "Prints all chunks in model MODEL subject to CONSTRAINTS.
See the function `filter-chunks' for a description of possible constraints."
  (print (mapcar 'chunk-name (if constraints
				 (search-for-chunks model constraints)
				 (declarative-memory-chunks (model-dm model))))))

(defun explain-activation (chunk-or-name &optional cues retr-spec)
  "Returns a string with an explanation of the evaluation of CHUNK.
CUES contains retrieval cues spreading activation.
RETR-SPEC describes the retrieval specification for partial matching retrievals."
  (when chunk-or-name
    (let ((chunk (get-chunk-object chunk-or-name)))
    (format nil "  time:~a  ~a base-level: ~a  (~a pres) pm: ~a ~a ~a"
	    (actUP-time)
	    (actup-chunk-name chunk)
	    (actup-chunk-get-base-level-activation chunk)
	    (actup-chunk-total-presentations chunk) ;; (actup-chunk-recent-presentations chunk)
	    (if cues
		(format nil "  spreading: ~a~%     ~a" (actup-chunk-get-spreading-activation chunk (get-chunk-objects cues 'noerror))
			;; explain
			(when (>= *debug* *all*)
			  (loop for cue in (get-chunk-objects cues) 
			     for link = (cdr (assoc (get-chunk-name chunk) (actup-chunk-related-chunks (get-chunk-object cue))))
			     
			     collect
			       (format nil "~a: ~a" (get-chunk-name cue)
				  (if  (and link (actup-link-sji link))
				       (format nil "Sji: ~a " (actup-link-sji link))
				       (format nil "Rji: ~a " (let ((rji (chunk-get-rji cue chunk))) (log-safe rji))))))))
		"")
	    (if retr-spec 
		(format nil "partial match: ~a " (actup-chunk-get-partial-match-score chunk retr-spec))
		"-")
	    (if *ans* (format nil "tr.noise: ~a " (actup-chunk-last-noise chunk)) "-")
	    ))))
  

(defmacro normalize-slotname (slot)
  `(intern (string-upcase (symbol-name ,slot))))
(defmacro normalize-slotname-with-package (slot)
  `(intern (string-upcase (symbol-name ,slot)) 'act-up))

(defun search-for-chunks (model args)
;  (say "searching for chunks ~a" args)
  (filter-chunks (model-chunks model) args))




(defun retrieve-chunk (spec &optional cues pm-soft-spec timeout)
  "Retrieve a chunk from declarative memory.
The retrieved chunk is the most highly active chunk among those in
declarative memory that are retrievable and that conform to
specification SPEC.

CUES is, if given, a list of chunks that spread activation
to facilitate the retrieval of a target chunk.  CUES may contain
chunk objects or names of chunks.

PM-SOFT-SPEC is, if given, a retrieval specification whose 
constraints are soft; partial matching is used for this portion
of the retrieval specification. 

SPEC and PM-SOFT-SPEC are lists of the form (:slot1 value1 :slot2
value2 ...), or (slot1 value1 slot2 value2).

TIMEOUT, if given, specifies the maximum time allowed before
the retrieval fails."
  (debug-print *informational* "retrieve-chunk:~%   spec: ~a~%  cues: ~a~%  pmat: ~a~%" spec cues pm-soft-spec)

  (let* ((matching-chunks (filter-chunks (model-chunks *current-actUP-model*)
					 spec))
	 (best-chunk (best-chunk matching-chunks
				 cues pm-soft-spec timeout)))
    (debug-print  *informational* "retrieved ~a out of ~a matching chunks.~%" (if best-chunk (or (actup-chunk-name best-chunk) "one") "none") (length matching-chunks))
    (debug-print *informational* "~a~%" (explain-activation best-chunk cues pm-soft-spec))
    ;; to do: add if to make fast
    (loop for c in matching-chunks do 
	 (debug-print *detailed* "~a~%" (explain-activation c cues (append spec pm-soft-spec))))
    best-chunk))


(defun blend-retrieve-chunk (spec &optional cues pm-soft-spec)
  "Retrieve a blended chunk from declarative memory.
The blended chunk is a new chunk represeting the chunks
retrievable from declarative memory under specification SPEC.
The contents of the blended chunk consist of a weighted average
of the retrievable chunks, whereas each chunk is weighted
according to its activation.

CUES is, if given, a list of chunks that spread activation
to facilitate the retrieval of target chunks. CUES may contain
chunk objects or names of chunks.

PM-SOFT-SPEC is, if given, a retrieval specification whose 
constraints are soft; partial matching is used for this portion
of the retrieval specification. 

SPEC and PM-SOFT-SPEC are lists of the form (:slot1 value1 :slot2
value2 ...), or (slot1 value1 slot2 value2)."
  (let ((cs (filter-chunks (model-chunks *current-actUP-model*)
			     spec)))
    (if cs
	(blend cs cues nil (append spec pm-soft-spec)))))
    

;; (defmethod slot-missing (class (object objc) slot-name 
;; 			 (operation (eql 'slot-value)) &optional new-value) 

;;   object slot-name operation new-value;; ignore
;;   'missing)

;; (normalize-slotname-with-package :name 'act-up)
(defun filter-chunks (chunk-set args)
  "Filter chunks according to ARGS.
ARGS is a list of the form (:slot1 value1 :slot2 value2 ...),
or (slot1 value1 slot2 value2).
CHUNK-SET is the list of chunks to be filtered (1), or an associative array (2)
of the form ((X . chunk1) (Y . chunk2) ...).
returns a list of chunks in case (1) and a list of conses in case (2)."

  ;(let ((csn nil))
  (loop for chunk-or-cons in chunk-set append
       (let ((c (if (consp chunk-or-cons) (cdr chunk-or-cons) chunk-or-cons)))
	 (handler-case
	     (if (loop for argval on args by #'cddr finally (return t) do
		      (let* ((slot ;(setq csn  
			      (normalize-slotname (first argval))) ;)
			     (vv (second argval))
			     (slot-value (slot-value c slot)))
			(unless (or (if (and (actup-chunk-p vv) (actup-chunk-p slot-value))
					(equal slot-value vv) 
					(eq (get-chunk-name vv) (get-chunk-name slot-value)))
				    (and (eq vv 'non-nil) slot-value))
			  (return nil))))
		 (list chunk-or-cons)
		 nil)
	   (error (_v) ;; (progn (debug-print *error* "Invalid slotname ~a in chunk ~a." csn (actup-chunk-name c) nil))
	     ;;   (print _v)
	     _v
	     nil ;; it's not really an error or a special situation
	     ;;   ;; we're going through all chunks that we have
	 )))))



(defparameter *actup--chunk-slots* (mapcar #'car (structure-alist (make-chunk)))
  "Internal to ACT-UP.")

(defvar *ol* 3  "Optimized Learning parameter for base-level learning in Declarative Memory.
OL is always on in ACT-UP.
See also: ACT-R parameter :ol")

(defparameter *associative-learning* nil ; would be 1.0 if on
  "The trigger for associative learning, a in ROM Equation 4.5.
   Can be any non-negative value.")
(export '(*ol* *associative-learning*))

(defun learn-chunk (chunk &optional co-presentations)
  "Learn chunk CHUNK.

This will note a presentation of an existing chunk in the model's DM, if
the existing chunk is unifiable with CHUNK.  If no such chunk exists in DM,
the CHUNK will be added.  If more than one such chunk exists, one of the existing
chunks is noted as 'presented'.

CHUNK may be altered by side-effect.

Returns the added chunk."

  (let* ((model *current-actUP-model*))

	 ;; because chunks must have names, we can directly search
	 ;; by name - we don't need to find a unifiable one.
	 ;; if a user has a prtial chunk spec, this API
	 ;; won't support it anyways.  (filter-chunks would have to be used.)
	 ;; (chunk-descr
	 ;;  (loop for slot in (cons 'name (slot-value chunk 'act-up::attrs)) append
	 ;;       (unless (member slot *actup--chunk-slots*)
	 ;; 	 (list slot (slot-value chunk slot)))))

	 ;; we're either taking the existing chunk, or we're adding a new one.

    ;; we must take care not to create a copy of the chunk object.
    (if (actup-chunk-p chunk)
	(if (not (member chunk (model-chunks model))) ;; use object in DM if present
	    (progn
	      ;; make sure we don't have a chunk of the same name already present
	      (let ((new-name (get-chunk-name chunk)))
		(and new-name
		     (get-chunk-by-name new-name)
		     (error (format nil "Another chunk of name ~s is already in DM when trying to learn chunk ~s." new-name chunk))))

	      ;; upon adding, always reset first presentation time
	      (setf (actup-chunk-first-presentation chunk) (actup-time))
	      (push chunk (model-chunks model))))
	     ; else: already a member.
	; else: get from DM by name
	; maybe learn implicitly?
	(setq chunk (get-chunk-by-name chunk)))   ;; retrieve by name
		      
    ;; (car (search-for-chunks model chunk-descr)) ;; use unifiable object 
    

    (incf (actup-chunk-total-presentations chunk))
    (push (actUP-time) (actup-chunk-presentations chunk))
    
    (when *associative-learning*
      (loop for c1 in co-presentations
	 for c = (get-chunk-object-add-to-dm c1) ;; make sure cue is in DM (to prevent user error)
	 do
	   (inc-rji-copres-count c chunk)
	   (inc-rji-copres-count chunk c))
      (incf (declarative-memory-total-presentations (model-dm model))))

    ;; update occurs-in lists in other (contained) chunks
    ;; OPTIMIZE: don't do this if chunk is already (unchanged) in DM
    (loop with name = (actup-chunk-name chunk)
       for slot in (slot-value chunk 'act-up::attrs)  ;; (structure-alist c) do
       for val = (slot-value chunk slot)
       when (symbolp val)
       do
	 (let ((ca (get-chunk-object-add-to-dm val)))
	   (when val
	     (pushnew name (actup-chunk-occurs-in ca)))))

    (push (actUP-time) (actup-chunk-recent-presentations chunk))
    (if (> (length (actup-chunk-recent-presentations chunk)) *ol*)
	(setf (cdr (nthcdr (1- *ol*) (actup-chunk-recent-presentations chunk))) nil)) ;; only OL 3

    ;; copy actup-chunk information
    ;; (when (actup-chunk-p chunk)
    ;;   (loop for slot1 in *actup--chunk-slots*
    ;; 	 for slot = (normalize-slotname-with-package slot1)
    ;; 	 do
    ;; 	   (setf (slot-value chunk slot) (slot-value chunk slot))))
    
    (pass-time *dat*) ;; 50ms
    chunk))

(defun get-chunk-objects (chunks-or-names)
  (loop for c in chunks-or-names append
       (let ((co (get-chunk-object c)))
	 (if co (list co) nil))))

(defun get-chunk-object (chunk-or-name &optional noerror)
  "Returns chunk object for CHUNK-OR-NAME.
If CHUNK-OR-NAME is a chunk, return is.
Otherwise, retrieves chunk by name CHUNK-OR-NAME
from current model DM.
Returns nil if NOERROR is non-nil, otherwise signals an error if chunk can't be found."
  (if (actup-chunk-p chunk-or-name)
      chunk-or-name
      (or (get-chunk-by-name chunk-or-name)
	  (if noerror
	      nil
	      (error (format nil "Chunk of name ~a not found in DM." chunk-or-name))))))

(defun get-chunk-object-add-to-dm (chunk-or-name)
  "Returns chunk object for CHUNK-OR-NAME.
Retrieves or creates chunk by name from current model DM
if CHUNK-OR-NAME is a symbol otherwise
returns CHUNK-OR-NAME.

If the object is not in the DM, add it."
  ;; we don't reuse get-chunk-object in order to not search the chunk set twice.
  (if (actup-chunk-p chunk-or-name)
      (progn 
	(if (not (member chunk-or-name (model-chunks (current-model))) )
	    ;; we must call learn-chunk to initialize its presentations etc.
	    (learn-chunk chunk-or-name))
	;;(push chunk-or-name (model-chunks (current-model)))
	chunk-or-name)
      (or (get-chunk-by-name chunk-or-name)
	  (let ((chunk (make-actup-chunk :name chunk-or-name)))
	    (debug-print *informational* "Implicitly creating chunk of name ~a.~%" chunk-or-name)
	    (learn-chunk chunk)
	    ;(push chunk (model-chunks (current-model)))
	    chunk
	  ))))

(defun get-chunk-name (chunk-or-name)
  (if (actup-chunk-p chunk-or-name)
      (actup-chunk-name chunk-or-name)
      chunk-or-name))


;; To Do:  use hash to speed this up 
(defun get-chunk-by-name (name)
  "Returns first chunks of name NAME"
  (loop for c in (model-chunks *current-actUP-model*)
     do
       (if (equal name (actup-chunk-name c))
	   (return c))))
(defun chunk-name-not-unique (name)
  "Returns non-nil if more than one chunk of name NAME exists.
Returns nil if name is nil."
  (unless (null name)
    (let ((num (loop for c in (model-chunks *current-actUP-model*)
		  when (equal name (actup-chunk-name c))
		  sum 1)))
      (if (> num 1)
	  num))))
       

(defparameter *lf* 1.0 "Latency Factor parameter for declarative retrieval time calculation.
See ACT-R parameter :lf")
(defparameter *le* 1.0  "Latency Exponent parameter for declarative retrieval time calculation.
See ACT-R parameter :le")
(export '(*lf* *le*))
(defun best-chunk (confusion-set cues &optional request-spec timeout &rest options)
"Retrieves the best chunk in confusion set.
CONFUSION-SET is a list of chunks, out of which the chunk is returned.
CUES is a list of cues that spread activation.  CUES may contain
chunk objects or names of chunks.
OPTIONS: do not use (yet).

Simulates timing behavior.

See also the higher-level function `retrieve-chunk'."

 ;; retrieve using spreading activation from cues to confusion-set
  (if confusion-set
      (let* ((last-retrieved-activation nil)
	     (cues (get-chunk-objects cues))
	     (below-rt-count 0)
	     (best  (loop  with bc = nil with bs = nil 
		       for c in confusion-set
		       when (if (eq options 'inhibit-cues) (not (member c cues)) t)
		       when c ;; allow nil chunks
		       do
			 (let ((s (actup-chunk-get-activation c cues request-spec)))
			   (if (or (not *rt*) 
				   (if (consp *rt*)
				       (> (length (actup-chunk-presentations c)) (cdr *rt*))
				       (> s *rt*)))
			       (when (or (not bc) (> s bs)) (setq bc c bs s))
			       (progn
				 (debug-print *informational* "chunk ~a's activation (~a) falls below RT (~a)~%" (get-chunk-name c) s *rt*)
				 (debug-print *detailed* "chunk ~a: ~a~%" (get-chunk-name c) c)
				 (incf below-rt-count))
			       ))
		       finally
			 (progn (setq last-retrieved-activation 
				      bs)
				(return bc))
			 )))

	;; timing
	(if last-retrieved-activation
	    (pass-time (* *lf* (exp (- (* *le* last-retrieved-activation))))))
	
	(let ((duration (* *lf* (exp (- (* *le* (or (if best last-retrieved-activation) *rt*)))))))
	  (debug-print *informational* "Retrieval duration: ~s~%" duration)
	  (if (and timeout (> duration timeout))
	      ;; time's up
	      (progn 
		(debug-print *informational* "Retrieval time-out ~a reached." timeout)
		(pass-time timeout) 
		nil)
	      ;; return nil
	      ;; timeout not given or within timeout
	      (progn
		(pass-time duration)
		best))))))

(defun best-n-chunks (n confusion-set &optional cues request-spec)
  "Retrieves the best chunks in confusion set.
CONFUSION-SET is a list of chunks, out of which the best N chunks will
be returned. CUES is a list of cues that spread activation.  CUES may
contain chunk objects or names of chunks.

See also the higher-level functions `retrieve-chunk' and
`blend-retrieve-chunk'."

  ;; retrieve using spreading activation from cues to confusion-set
  (if confusion-set
      (let* ((cues (get-chunk-objects cues))
	     (all  (loop for c in confusion-set 
		  append
		    (let ((s (actup-chunk-get-activation c cues request-spec)))
		     
		      (if (or (not *rt*) 
			      (if (consp *rt*)
				  (> (length (actup-chunk-presentations c)) (cdr *rt*))
				  (> s *rt*)))
			  (list (cons s c)))))
			  ))
	(mapcar 'cdr (subseq (stable-sort all #'> :key #'car) 0 (min n (length all) ))))))

 
(defparameter *blend-temperature* 1.0)

(defun blend (chunks &optional cues chunk-type retrieval-spec)
  "Return a blended variant of chunks.
Activation is calculated using spreading activation from CUES.  CUES
may contain chunk objects or names of chunks.  The returned chunk is
of type CHUNK-TYPE; all CHUNKS must be of type CHUNK-TYPE or of a
supertype thereof.  If CHUNK-TYPE is not given, all CHUNKS must be of
the same class and the returned type will be this class.
RETRIEVAL-SPEC should contain the retrieval filter used to obtain
CHUNKS; attribute-value pairs in it will be included in the returned
chunk as-is and not be blended from the CHUNKS.

See also the higher-level function `blend-retrieve-chunk'."
  

  
  (if (and chunk-type (symbolp chunk-type))
      (setq chunk-type (find-class chunk-type)))

  (debug-print *informational* "Blending ~a different chunks..." (length chunks))

  ;;convert flat list into assoc list
  (let ((cues (get-chunk-objects cues))
	(auto-chunk-type nil) 
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

	 (let ((act (actup-chunk-get-activation c cues retrieval-spec)))
	   (setq blend-activation (+ blend-activation
				     (exp act)))
	   (loop for slot in  (slot-value c 'act-up::attrs)  ;; e.g., 'error
	      do
		(when (and (not (assoc slot retrieval-spec-alist))
			   ;; not an internal slot from "chunk"
			   ;; don't think we still need this
			   ;; (not (slot-exists-p empty-chunk (normalize-slotname-with-package slot)))
			   ;;   (not (member (normalize-slotname-with-package (car s)) chunk-standard-slots))
			   )
		  (if (not (assoc slot slot-values-by-name))
		      (push (cons slot nil) slot-values-by-name))
		  (push
		   (cons act (slot-value c slot)) ;; value
		   (cdr (assoc slot slot-values-by-name)))))))
    
    (setq blend-activation (log-safe blend-activation 0))
    
    ;; let's blend
    ;; currently, only numerical values are supported    
    
    
    ;; to do: return real object
    ;; (class-of c) ??
    ;; (make-instance class ...)
    (debug-print *informational* "blending ~a slots" (length slot-values-by-name))
    (debug-print *detailed* "~a" slot-values-by-name)

    (if (> blend-activation *rt*)
	;; (apply #'make-instance    does not work for structures (openmcl)
	;; 	      chunk-type 
	(if (or retrieval-spec slot-values-by-name)
	    (apply (find-symbol (format nil "MAKE-~a" (class-name chunk-type)))
		   :last-bl-activation blend-activation
		   :activation-time (actup-time)
		   :comment 'blended
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
			   (when sum
			     (list (find-symbol (symbol-name (car sv)) "KEYWORD")  ;; 'error-1 --> :error-1
				   (/ sum boltzmann-total)))))))
	    (progn
	      (debug-print *warning* "No retrieval spec given, and no slots found to be blended.  Blending failed.")
	      nil))
	(progn
	  (debug-print *warning* "Resulting blend activation ~a below retrieval threshold (*rt*=~a).  Blending failed." blend-activation *rt*)
	  nil))))

(defun reset-mp ()
  "Resets the current Meta process. 
Resets the time in the meta process."
  (setq *current-actUP-meta-process* (make-meta-process)))

(defun reset-model ()
  "Resets the current ACT-UP model. 
All declarative memory and all subsymbolic knowledge is deleted.
Global parameters (dynamic, global Lisp variables) are retained, as are
functions and model-independent rules."
  (setq *current-actUP-model* (make-model)))

;; Associative learning

;; Leaving AL on by default would be tricky:
;; if chunks have no joint presentations or learn-chunk doesn't get the co-present chunks,
;; then with every single presentation of a chunk, the Rji will decline rapidly
;; (number f_c is in denominator!)
;; thus we start out with a nice fan effect (Rji prior), but end up with low Sjis.

;; So that's not ideal.  That's why AL is off by default. 

(defun inc-rji-copres-count (c n)
  "increase co-presentation count for chunks C,N"
  (let ((target (cdr (assoc (get-chunk-name n) (actup-chunk-related-chunks c)))))
    (if target
	(incf (actup-link-fcn target)) ;; f(C&N) count
	(let ((link (make-actup-link :fcn 1)))
	  ;; add new link:
	  (setf (actup-chunk-related-chunks c)
		(cons
		 (cons (get-chunk-name n) link)
		 (actup-chunk-related-chunks c)))
	  ;; same link in the reciprocal references
	  (setf (actup-chunk-references n)
		(cons
		 (cons (get-chunk-name c) link)
		 (actup-chunk-references n)))))))

(defparameter *maximum-associative-strength* 1.0 "Maximum associative strength parameter for Declarative Memory
See also `*associative-learning*', `reset-sji-fct'.
See also: ACT-R parameter :mas.")
(define-symbol-macro *mas* *maximum-associative-strength*) ; compatibility macro
(export '(*maximum-associative-strength* *mas*))

(defmacro count-occ-ji (val chunk)
  `(count-if (lambda (slot)
		     (eq ,val (slot-value ,chunk slot))) 
	     (slot-value ,chunk 'act-up::attrs)))

;; we're counting all mentions of a chunk in order to calculate the fan
;; perhaps we should just add to a fan count whenever a novel chunk is added to DM
;; and treat chunk contents as "readonly" once they're in DM (which they should be)
;; this solution is probably quite slow

(defun count-occurrences (chunk)
  "Count occurrences of CHUNK as value in all other chunks"
  (loop with name = (actup-chunk-name chunk)
     for cn in (actup-chunk-occurs-in chunk) 
     for c = (get-chunk-object cn)
     sum
       (count-occ-ji name c)))

(defun fan-ji (c n)
  (let ((occ (count-occ-ji (get-chunk-name c) n)))
    ;; (format t "fan-ji: c:~s n:~s  occ:~s  occs: ~s~%" c n occ (count-occurrences c))
    (if (> occ 0)
	(/ (1+ (count-occurrences c))
	   (+ occ
	      (if (eq c n) 1 0)))
	
	nil)))

(defun chunk-get-rji-prior (c n) ;; c=j, n=i
  "Get Rji prior (in linear space)"
;; m/n
; The fan is s_ji = S - log(fan_ji)
; so, it is s_ji = log(e^S/fan_ji)

  (let ((fan (fan-ji c n)))
    (if fan
	(/ (if (numberp *maximum-associative-strength*)
	       (exp *maximum-associative-strength*)  ; MAS is in log space
	       (length (model-chunks (current-model))))
	   fan)  ; num refs for context c
	1)))

(defun chunk-get-rji (c n)
  "Get Rji (in linear space)"
  (if *associative-learning*
      (let ((target (cdr (assoc (get-chunk-name n) (actup-chunk-related-chunks c)))))
	(if target
	    (let ((no (get-chunk-object n)))
	      (let ((f-nc (or (actup-link-fcn target) 0))
		    ;; 1+ in order to make it work even without presentations
		    (f-c (1+ (actup-chunk-total-presentations (get-chunk-object c))))
		    (f-n (1+ (actup-chunk-total-presentations no)))
		    (f (/ (- (actup-time) (actup-chunk-first-presentation no)) *dat*)))  ;; this is #cycles in ACT-R 5
		(if (and (> f-c 0) (> f 0) (> f-n 0))
		    (let* ((pe-n-c (/ f-nc f-c))
			   (pe-n (/ f-n f))
			   (e-ji (/ pe-n-c pe-n)))
		      ;; Bayesian weighted mean between prior and E
		      (/ (+ (* *associative-learning* (chunk-get-rji-prior c n))
			    (* f-c e-ji))
			 (+ *associative-learning* f-c)))
		    1)))
	    ; should this be 0, or the prior??
	    1))
      ;; this implies the chunk's fan:
      (chunk-get-rji-prior c n)))
    


;; ACT-R 6.0 compatibility functions
(defun reset-sji-fct (chunk)
  "Removes all references to CHUNK from all other chunks in the current model."
 
  ;;; remove chunk from reciprocal references 
  (loop with chunk-name = (get-chunk-name chunk)
     for (c . _l) in (actup-chunk-related-chunks chunk) do
       (progn
	 _l
	 (setf (actup-chunk-references c)
	       (delete-if (lambda (x) (eq (car x) chunk-name)) (actup-chunk-references c)))))
  (setf (actup-chunk-related-chunks chunk) nil))


;; (defun add-sji-fct (list)
;;   (loop for (c1a c2a s) in list 
;;      for c1 = (get-chunk-object c1a)
;;      for c2 = (get-chunk-object c2a)
;;      do
;;        (unless (eq c1 c2)
;; 	 (setf (actup-chunk-related-chunks c1) 
;; 	       (delete (assoc c2 (actup-chunk-related-chunks c1)) 
;; 		       (actup-chunk-related-chunks c1)))
;; 	 (setf (actup-chunk-related-chunks c1)
;; 	       (insert-by-weight (list c2 s 0 0) (actup-chunk-related-chunks c1) #'second)))
;;        ))

(defun alist-replace (key value alist)
  "Replace key-value pair in alist, or add it.
Alist is changed by side-effect and returned,
unless alist is NIL, in which case Alist is
not changed, but the new alist containing the
key-value pair is returned."
  (if alist
      (loop with last = nil
	 for c on alist 
	 for x = (car c) do
	   (when (eq (car x) key)
	     (setf (cdr x) value)
	     (return))
	   (setq last c)
	 finally 
	   (setf (cdr last) (cons (cons key value) nil)))
      (setq alist (cons (cons key value) nil)))
  alist)

;; s - sji
;; value
;; or (Fjoint time)  ; time unused in the ACTR5 style calculation

(defun set-similarities-fct (list)
  "Set similarities between chunks.
LIST is a list with elements of form (A B S), where A und B are
chunks or chunk names, and S is the new similarity of A and B.
For example:

 (set-similarities-fct '((dave david -0.05) 
                         (steve hank -0.1)  
                         (mary john -0.9)))"

  (loop for (c1a c2a s) in list 
     for c1 = (get-chunk-object-add-to-dm c1a)
     for c2 = (get-chunk-object-add-to-dm c2a)
     for c1n = (get-chunk-name c1)
     for c2n = (get-chunk-name c2)
     do
       (unless (eq c1 c2)

	   (setf (actup-chunk-similar-chunks c1)
		 (alist-replace c2n s (actup-chunk-similar-chunks c1)))
	   (setf (actup-chunk-similar-chunks c2)
		 (alist-replace c1n s (actup-chunk-similar-chunks c2)))
	   )))

(defun add-sji-fct (list)
  "Set Sji link weights between chunks.
LIST is a list with elements of form (CJ NI S), where CJ und NI are
chunks or chunk names, and S is the new link weight, regulating
spreading activation when CI is in context as a cue and NI is
retrieved.  S may also be a list of form (FCN TIME), with FCN
indicating frequency of C and N occurring together, and TIME
indicating the point in time of their last joint occurrence (TIME is
unused currently, but must be given.)"
  (loop for (c1a c2a s) in list 
     for c1 = (get-chunk-object-add-to-dm c1a)
     for c2 = (get-chunk-object-add-to-dm c2a)
     for c1n = (get-chunk-name c1)
     for c2n = (get-chunk-name c2)
     do
       (unless (eq c1 c2)
	 (let ((link (if (and (listp s) (= (length s) 2))
			 (make-actup-link :fcn (first s))
			 (make-actup-link :sji s))))
	   (setf (actup-chunk-related-chunks c1)
		 (alist-replace c2n link (actup-chunk-related-chunks c1)))
	   ;; and the reciprocal references
	   (setf (actup-chunk-references c2)
		 (alist-replace c1n link (actup-chunk-references c2)))
	   ))))

(defun set-dm-total-presentations (npres)
  "Set the count of total presentations of all chunks in DM.
This value is relevant for associative learning (Sji/Rji)."
  (setf (declarative-memory-total-presentations (model-dm (current-model))) npres))

(defun set-base-level-fct (chunk value &optional creation-time)
  "Set base levels of CHUNK.
If CREATION-TIME is specified, it contains the time at which the chunk
was created in declarative memory, and VALUE contains the number of
presentations (an integer value).  If TIME is not specified, VALUE is
the chunk's absolute activation value (log space).

For plausibility reasons, models should specify presentations and time
when possible."
  
    (let ((c (get-chunk-object-add-to-dm chunk)))
      (if creation-time
	  (let* ((presentations value)
		 (age (- (actUP-time) creation-time))
		 (mpres presentations))
	    ;; mpres = (min presentations *ol*) ;  (ACT-R variant)
	    ;; the "min" is here for ACT-R 6 compatibility.  It doesn't make sense, really.
	    (setf (actup-chunk-total-presentations c) presentations)
	    (setf (actup-chunk-recent-presentations c) 
		  (loop for i from 1 to *ol* collect
		       (- (actUP-time) (* i (/ age mpres)))))
	    (setf (actup-chunk-first-presentation c) creation-time))
	  ;; old-style form
	  (setf (actup-chunk-last-bl-activation c) value))))


(defun set-base-levels-fct (list)
  "Set base levels of several chunks.
ACT-R compatibility function.
LIST contains elements of form (CHUNK PRES TIME) or (CHUNK ACT),
whereas CHUNK is a chunk object or the name of a chunk,
PRES is a number of past presentations (integer),
and TIME the life time of the chunk,
and ACT the chunk's absolute activation.

For plausibility reasons, models should not use the ACT form when
possible."
 
  (loop for el in list do
       (apply #'set-base-level-fct el)))

(defmacro chunks ()
  '(model-chunks (current-model)))



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
 

(defun actup-chunk-get-noise (chunk)
  (if *ans* 
      (or (and (eq (actUP-time) (actup-chunk-last-noise-time chunk))
	       (actup-chunk-last-noise chunk))
	  (progn
	    (setf (actup-chunk-last-noise chunk) (act-r-noise *ans*)
		  (actup-chunk-last-noise-time chunk) (actUP-time))
	    (actup-chunk-last-noise chunk)))
      0))

(defun actup-chunk-get-activation (chunk &optional cue-chunks retrieval-spec)
  "Calculate current activation of chunk"

  (let ((base-level (actup-chunk-get-base-level-activation chunk))
	(spreading (actup-chunk-get-spreading-activation chunk cue-chunks))
	(partial-matching (actup-chunk-get-partial-match-score chunk retrieval-spec))
	(noise (actup-chunk-get-noise chunk)))
	
	(+ base-level spreading partial-matching noise)))


;; export

    

; to do
; relax "member" - can't work like this
; update activation
; update references
; initialize name if necessary
; 


   ;; set time


;   (setf (declarative-memory-chunks (model-dm (current-model))) chunks)


(defun bll-sim (pres-count lifetime)
  (+ *blc* (log (/ pres-count (- 1 *bll*))) (- (* *bll* (log lifetime)))))

(defun actup-chunk-get-base-level-activation (chunk)

  ;; we're using the Optimized Learning function

					;(let ((d *bll*)) ;; (model-parameters-bll (model-parms (current-model)))

  (+
   ;; initial BL activation  (e.g., from blending, or from base-level constant)
   (or (actup-chunk-last-bl-activation chunk) *blc*)

   (if *bll*
       (let ((time (actUP-time))
	     (1-d (- 1 *bll*)))
	 (log-safe
	  (+
	   ;; standard procedure
	   (loop for pres in (actup-chunk-recent-presentations chunk) sum
		(progn
		  (format t "adding ~a~%" (- time pres))
		(expt (max 1 (- time pres)) (- *bll*))))
	   
	   (let ((k (length (actup-chunk-recent-presentations chunk))))
	     (if (and (> (actup-chunk-total-presentations chunk) k) (actup-chunk-first-presentation chunk))
		 ;; optimized learning
		 (let ((last-pres-time (max 1 (- time (or (car (last (actup-chunk-recent-presentations chunk))) 
							  (actup-chunk-first-presentation chunk))))) ;; 0? ;; tn
		       (first-pres-time (max 1 (- time (actup-chunk-first-presentation chunk)))))
		   (if (and first-pres-time
			    (not (= first-pres-time last-pres-time)))
		       (progn
			 (/ (* (- (actup-chunk-total-presentations chunk) k) 
			       (max 0.1 (- (expt first-pres-time 1-d) (expt last-pres-time 1-d))))
			    (* 1-d (max 0.1 (- first-pres-time last-pres-time)))))
		       0.3))
		 ;; fall back to default decay
		 ;; (let ((last-pres-time (max 1 (- time (or (car (last (actup-chunk-recent-presentations chunk))) 
;; 		 						       (actup-chunk-first-presentation chunk))))) ;; 0? ;; tn
;; 		 	 (first-pres-time (max 1 (- time (actup-chunk-first-presentation chunk)))))
;; 		   (if (and first-pres-time
;; 		 	      (not (= first-pres-time last-pres-time)))
;; 		 	 (progn
;; 		 	   (/ (* (- (actup-chunk-total-presentations chunk) k) 
;; 		 		 (max 0.1 (- (expt first-pres-time 1-d) (expt last-pres-time 1-d))))
;; 		 	      (* 1-d (max 0.1 (- first-pres-time last-pres-time)))))
;; 		 	 0)
		   
		 0))))) ; !!!
       0)
   ))

(defun actup-chunk-get-spreading-activation (chunk cues)
  (if cues
      (* 1
	 (/ (loop 
	       for cue in cues
	       for link = (cdr (assoc (get-chunk-name chunk) (actup-chunk-related-chunks (get-chunk-object cue))))
	       sum
		 (+ (or 
		     (if *associative-learning*
			 (+ (if link (actup-link-sji link) 0) ; add on Sji (is this the right thing to do?)
			    (let ((rji (chunk-get-rji cue chunk))) (log-safe rji)))
			 ;; assoc learning is off:
			 (or
			  (if link (actup-link-sji link)) ;; Sji
			  ;; get Rji (prior) for fan effect if Sji isn't set
			  (let ((rji (chunk-get-rji cue chunk))) 
			    ;; convert RJI to log space!
			    (log-safe rji nil))))
		     ;; this doubles the lookup in related chunks - to revise!:
		     0))) ;; Rji (actup-link-rji link)
	    (length cues)))
      0))


(defparameter *mp* 1.0 "ACT-UP Partial Match Scaling parameter
Mismatch (`set-similarities-fct') is linearly scaled using this coefficient.")

(defparameter *ms* 0 "ACT-UP Partial Match Maximum Similarity
Similarity penalty assigned when chunks are equal.
Value in activation (log) space.")

(defparameter *md* -1 "ACT-UP Partial Match Maximum Difference
Similarity penalty assigned when chunks are different
and no explicit similarity is set.
Value in activation (log) space.")

(export '(*mp* *ms* *md*))
(defun actup-chunk-get-partial-match-score (chunk retrieval-spec)
  (if *mp*
      (progn ; (print retrieval-spec)

	(* *mp*
	   (loop for (s v) on retrieval-spec  by #'cddr sum
		(value-get-similarity (slot-value chunk (normalize-slotname s)) v))
	   ))

      ;; else
      0))

(defun value-get-similarity (v1 v2) 
  (or 
   (let ((v1o (get-chunk-object v1 'noerror)))
     (if v1o
	 (cdr (assoc (get-chunk-name v2) (actup-chunk-similar-chunks v1o)))))
   (if (eq (get-chunk-name v1) (get-chunk-name v2)) *ms*)
      ;; (if (and (numberp v1) (numberp v2))
      ;; 	  (if (= v1 v2)
      ;; 	      *ms*
      ;; 	      (+ *ms* (* (- *md* *ms*) (abs (- v1 v2)))))  ;; could be done better!
      ;; 	  )
      ; unrelated chunks
   *md*))


;; PROCEDURAL

;; tests

;; (setq *actup-rulegroups* nil)
;; (defrule rule1 (arg1 arg2) :group g1 (print arg1))
;; (defrule rule1b (arg1 arg2) :group (g1 g5) (print arg1))
;; (defrule rule2 (arg2 arg3) :group (g1 g2) (print arg1))
;; (defrule rule3 (arg3 arg4) :group g2 (print arg1))
;; (equal *actup-rulegroups* '((G2 (RULE3 ARG3 ARG4) (RULE2 ARG2 ARG3)) (G5 (RULE1B ARG1 ARG2)) (G1 (RULE2 ARG2 ARG3) (RULE1B ARG1 ARG2) (RULE1 ARG1 ARG2))))
;; (g1 'working 'huh)

(defun set-alist (key val alist)
  "Sets value in alist."
  (let ((kv (assoc key alist)))
    (if kv
	(rplacd kv val)
	(setf (cdr (last alist)) (list (cons key val)))))
  alist)


(defstruct rule
  name  ; also function name of rule
  utility
  firing-time  ; :at parameter in ACT-R
)

(defstruct (compiled-rule (:include rule))
  result
  args
  original-rule)

(defmacro defrule (name args &rest body)
  "Define an ACT-UP rule.
The syntax follows the Lisp `defun' macro, except that 
some keyword-argument parameters may follow ARGS
at the beginning of BODY.

This macro will define a Lisp function of name NAME with
arguments ARGS.  The Lisp function will execute the Lisp
forms in BODY and return the value of the last form.

The known parameters are:

 :GROUP the-group
A :group parameter defines one or or a list of rule
groups that the rule will belong to.  All rules defined as part of a
group must have the same argument footprint.


If GROUP is given, a function of name GROUP will also be
defined that invokes one of the rules assigned to GROUP.
For example:

 (defrule subtract-digit-by-addition (minuend subtrahend)
   :group subtract
   \"Perform subtraction of a single digit via addition.\"
   (let ((chunk (retrieve-chunk `(:chunk-type addition-fact
                                  :result ,minuend
                                  :add1 ,subtrahend))))
       (if chunk (addition-fact-add2 chunk))))
 (defrule subtract-digit-by-decrement (minuend subtrahend)
   :group subtract
   \"Perform subtraction of a single digit via subtraction knowledge.\"
   ...)

These rules can be invoked via a function call such as

 (subtract 5 2)

ACT-Up will choose the rule that has the highest utility.  See
`assign-reward' for manipulation of utilities (reinforcement
learning), and `*rule-compilation*' for in-theory compilation of
rules (routinization, internalization).

 :INITIAL-UTILITY u

The :initial-utility parameter sets the utility that this rule receives when it is created or the model is reset.
If not given, the initial utility will be the value of `*iu*' at time of first invocation.

Rule utilities, wether initial or acquired through rewards are always specific to the model.

Rules and groupings of rules are not specific to the model."
  ;; remove keyword args from body
  (let* (
	 (args-filtered (loop for a in args 
			     when (or (consp a)
				      (and (symbolp a)
					   (not (eq a '&optional))
					   (not (eq a '&key))))
			     collect
			     (if (consp a)
				 (car a)
				 a)))
	 (doc-string "Invoke ACT-UP rule.")
	 iu
	 (groups
	 (let (group)
	   (loop for keyw = (car body) do
		(cond
		  ((stringp keyw)
		   (setq doc-string keyw))
		  ((eq keyw :group)
		   (pop body)
		   (if group 
		       (error 
			(format nil "defrule ~s: more than one :GROUP keyword given."
				name)))
		   (setq group (car body)))
		  ((or (eq keyw :initial-utility) (eq keyw :iu))
		   (pop body)
		   (if iu 
		       (error 
			(format nil "defrule ~s: more than one :INITIAL-UTILITY keyword given."
				name)))
		   (setq iu (car body)))
		  ((keywordp keyw) t)
		  (t (return nil))
		  )
		(pop body)		 
		) (if (consp group) group (list group)))))
    (if (member name groups)
	(error (format nil "defrule: rule name ~a must not coincide with group name."
		       name)))
    (if (eq 'quote (car groups))
	(setq groups (second groups)))

    `(progn
    (defun ,name ,args
       ,doc-string
;; to do: handle signals
       (let (
	     (actup---rule (actup-rule-start ',name ,(cons 'list args-filtered) ,iu))
	     (actup---rule-result
	      (progn
		,@body)))
	 (actup-rule-end actup---rule ',(or groups (list name))
			 ,(cons 'list args-filtered) actup---rule-result)
	 actup---rule-result))
    (declare-rule ',groups
		   ',name ',args)
)))

(defun actup-rule-start (name args initial-utility)
  (declare (ignore args))
  ;;(format t "start: ~s ~s" name args)

  ;; look up rule object
  (let ((rule (lookup-rule name initial-utility)))
    ;; add rule to queue
    (push (cons (actup-time) rule)
	  (procedural-memory-rule-queue (model-pm (current-model))))
    
    ;; return rule
    rule))

(defun make-compiled-rule-name (group args result)
  (intern (format nil "~a/~a->~a"
		  group (length args)
		  (cond ((symbolp result) result)
			((numberp (format nil "~2f" result)))
			((actup-chunk-p result) (get-chunk-name result))
			(t "*")))))


(defparameter *rule-compilation* nil "If non-nil, rule compilation is enabled.
Rule compilation causes ACT-UP rules defined with `defrule' to be compiled (or: cached).
After execution of a source rule,  name, execution arguments and the result are stored as
compiled rule.  The compiled rule is added to each of the source rule's groups.

When the group is executed, compiled rules compete for execution with the other rules in the group.  (The rule with the highest utility is chosen.)

The initial utility of a compiled rule equals the initial utility of the source rule.  When a source rule is compiled multiple times, the utility of the compiled rule is updated by assigning the source rule utility as reward to the compiled rule (according to the ACT-R difference learning equation).  See also `assign-reward' for reward assignment to regular rules.")
(export '(*rule-compilation*))

(defun actup-rule-end (this-rule groups args result)

  ;; possibly compile this rule

  ;; compile rule
  (when *rule-compilation*
    (loop for group in groups 
       for leaf = (get-tree-leaf-create (procedural-memory-compiled-rules 
					 (model-pm (current-model)))
					(cons group args))
	 do
	 (or
	  ;; rule already present in list:
	  (loop for rule in (cdr leaf) do
	       
	       (when (equal result (compiled-rule-result rule))
		 ;; update utility of this rule
		 (assign-reward-to-rule (rule-utility this-rule) rule)
		 (return t)))
	  (setf (cdr (last leaf))
		(list 
		 (make-compiled-rule
		  :name (make-compiled-rule-name group args result)
		  :args args
		  :result result
		  :utility (rule-utility this-rule)
		  :original-rule (rule-name this-rule)
		  :firing-time (rule-firing-time this-rule)))))))

  ;; (format t "end: ~s ~s" name result)
  (pass-time (or (rule-firing-time this-rule) *dat*)) ;; to do: randomization (:vpft parameter)
)

;; this is, as of now, independent of the model
(defparameter *actup-rulegroups* nil)


(defparameter *au-rpps* 0.05
  "Reward proportion per second elapsed.
e.g., after 10 seconds we want to assign 50% of the remaining reward: 0.5/10 = 0.05
time is in between rules.
See also the parameter `*au-rfr*' and the function `assign-reward'.")

(defparameter *au-rfr* 0.10
  "base reward proportion for each rule
e.g., the each rule before the reward trigger gets 10% of the reward.
See also the parameter `*au-rpps*' and the function `assign-reward'.")

(defparameter *alpha* 0.2  "utility learning rate
See also the function `assign-reward'.")

(defparameter *iu* 0.0 "initial utility
See also the function `assign-reward'.")

(defparameter *egs* nil "Transient noise parameter for ACT-UP rules.
See also: ACT-R parameter :egs")

(export '(*au-rpps* *iu* *alpha* *au-rfr* *iu* *egs* assign-reward))

(defun declare-rule (groups name args)
  ;; to do: check number of arguments 
  (when groups
    (loop for g in groups when g do
	 ;; (re)define lisp function with group name
	 (eval `(defun ,g ,args 
		  ,(format nil "Choose a rule out of group %s" g)
		  (actup-eval-rule ',g ,@args)))

	 (let ((group-cons (assoc g *actup-rulegroups*)))
	   (if group-cons
	       (unless (member name (cdr group-cons))
		 (setf (cdr group-cons)  
		       (cons name (cdr group-cons))))
	       
	       (setq *actup-rulegroups*
		     (cons 
		      (list g
			    name)
		      *actup-rulegroups*)))))))

  

;; we cannot use a normal hash
;; because sxhash is simply not very good with objects
;; (only seems to depend on object type)
;; (defun rule-result-hash (name args)

;;   ;; problem here:
;;   ;; arguments could be chunks
;;   ;; which, internally, can be very different

;;   (sxhash (list name args)))

;; test case:
;; (defun rule2 (a1 a2) (print a1))
;; (actup-eval-rule 'g2 1 2)


    

(defun fire-compiled-rule (rule)
  (pass-time 0.05)
  (compiled-rule-result rule))

(defun lookup-rule (name &optional initial-utility)
  "Look up a rule in current model PM from rule name.
Add a rule object to current model PM if necessary."

  ;; look up rule object
  (let* ((regular-rules (procedural-memory-regular-rules (model-pm (current-model))))
	 (rule (gethash name regular-rules)))

    (unless rule
      (setq rule (make-rule :name name :utility initial-utility))
      (setf (gethash name regular-rules) rule))
    rule))
    
(defun actup-eval-rule (group &rest args)
  "Evaluates an ACT-UP rule from rule group GROUP.
Passes arguments ARG to the lisp function representing 
the chose rule."
  ;; chose rule with highest utility
  ;; must randomize choice of rule even if *egs* is nil
  (let ((regular-rules (mapcar #'lookup-rule (cdr (assoc group *actup-rulegroups*))))
	(compiled-rules 
	 ;; the leaf value from the tree structure is list of COMPILED-RULE structures
	 (get-tree-value (procedural-memory-compiled-rules (model-pm (current-model)))
			 (cons group args))))
    (unless regular-rules
      (error (format nil "No rules defined for group ~a." regular-rules)))                                                              
    (let* ((rules)
	   (rule-util
	 
		 (loop with utility = -100000.0
		    for rule in (append regular-rules compiled-rules)
		    for r-utility = (+ (or (rule-utility rule) *iu*) 
				       (if *egs* (act-r-noise *egs*) 0.0))
		    when (>= r-utility utility)
		    do
		      (setq rules  (if (> r-utility utility) 
				       (list rule)   ; better
				       (cons rule rules)) ; as good as others
			    utility r-utility)
		    finally
		      (return (cons utility (choice rules)))))
	   (rule (cdr rule-util)))
      (debug-print *informational* "Group ~a, ~a~a matches, choosing rule ~a (Utility ~a) from subset of best ~a~%"
		   group 
		   (length regular-rules)
		   (if *rule-compilation*
		       (format nil "+~a" (length compiled-rules)))
		   (rule-name rule) (first rule-util) (length rules))
      (when rule
	(if (compiled-rule-p rule) ;; compiled rule?	    
	    (fire-compiled-rule rule) ;; (car rule) is result
	    ;; regular rule:
	    (apply (rule-name rule) args))))))


;; just a linear backpropagation over time
; quue elements: (time . hash)
(defun assign-reward (reward)
  "Assign reward to recently invoked rules.
Distributes reward value REWARD across the recently invoked rules.
See parameters `*au-rpps*', `*au-rfr*', `*alpha*', and `*iu*'.
See `defrule' for documentation on how to use utility when
selecting between rules.

Reward must be greater than 0."
  (let ((last-time (actup-time)))
    (debug-print *informational* "Assigning reward ~a~%" reward)
    (loop for rc in (procedural-memory-rule-queue (model-pm (current-model))) do
	 (let* ((r-time (car rc))
		(r-rule (cdr rc))
		(reward-portion (* reward
				   (+ *au-rfr*
				      (* *au-rpps* (- last-time r-time))))))
	   (if (< reward-portion 0) (return nil))
	   (setq reward (- reward reward-portion)
		 last-time r-time)
	   (assign-reward-to-rule reward-portion r-rule)  ;; assign reward
	   ))
    ;; (setf (procedural-memory-rule-utilities (model-pm (current-model)))
    ;; 	  utilities)
    ))

(defun assign-reward-to-rule (reward-portion rule)
  "Assign reward to a specific rule."
  ;; assign reward
	   (let* ((current (or (rule-utility rule) *iu*))
		  (scaled (* *alpha* (- reward-portion current))))
	     (debug-print *informational* "Assigning reward ~a to ~a.~%" scaled (rule-name rule))
	     (setf (rule-utility rule)
		   (+ current scaled))
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



;; MODULES

;; locations used by visual and manual model
(define-chunk-type location 
    screen-x
  screen-y)

(load (format nil "~a/au-visual.lisp" (directory-namestring *load-truename*)))
(load (format nil "~a/au-manual.lisp" (directory-namestring *load-truename*)))




(provide "act-up")