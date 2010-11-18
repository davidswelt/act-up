;; ACT-UP
;; (C) Carnegie Mellon University
;; (C) David Reitter, 2010

;; reitter@cmu.edu

;; to load:
;; (require "act-up" "act-up.lisp")
;; (use-package :act-up)


;; (if (boundp 'emacs-version)
;;     load (format "%s../util/emacs-lisp-compat.el"  (file-name-directory (or load-file-name default-directory))))
;;     (load (format nil "~a/../util/emacs-lisp-compat.el" (directory-namestring *load-truename*))))



(declaim (optimize (speed 0) (space 0) (debug 03)))

;; avoid some error messages 
;;(let ((*error-output* (MAKE-STRING-OUTPUT-STREAM)))

;; must be top-level to avoid compile-time errors in ccl.
(defpackage :act-up
  (:documentation "The ACT-UP library.  Defines a number of functions
and macros implementing the ACT-R theory (Anderson 1993, Anderson et al. 1998,
Anderson 2007, etc.).
 (C) 2010, David Reitter, Carnegie Mellon University.")
  (:use :common-lisp))


(in-package :act-up)


;; prevent some issues
(setq *print-level* 6)


;; Emacs Lisp compatibility
(defun format-nil (form &rest args)
  (apply #'format nil form args))

(defun format-t (form &rest args)
  (apply #'format t form args))

;; `actup-load' command:
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *act-up-avoid-multiple-loading* nil)  ;; must be top-level to avoid compile-time errors in ccl.
  (unless (find-symbol "actup-load")
    (let* ((*act-up-avoid-multiple-loading* 'is-loading)
	   (file (directory-namestring (or *load-truename* *compile-file-truename*)))
	   (file-1 (concatenate 'string file "load-act-up.lisp"))
	   (file-2 (concatenate 'string file "../load-act-up.lisp")))
      ;; LW sets *compile-file-truename* to the name of the file being compiled, which
      ;; does not have to be this one.
      (load (if (probe-file file-1) file-1 file-2)))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (actup-load "actr6-compatibility")
  (actup-load "actr-aux")
  (actup-load "act-up-util"))

;; declarations

(DECLAIM (FTYPE (FUNCTION (number) number) act-r-noise))


;;; ACT-UP parameter system

(defparameter *actup-parameters* nil
  "List of ACT-UP parameters
Each element is of form (name init-form init-value module).")

(defmacro def-actup-parameter (name init-value &optional doc-string module)
  "Define ACT-UP parameter.
Macro internal to ACT-UP. Modifies `*actup-parameters*'.
Upon reset, ACT-UP initializes each parameters of NAME with 
the result of evaluating INIT-FORM.
Internal parameters should be marked with MODULE `internal'.
These are not shown by `show-parameters'." 
  `(progn
     (defparameter ,name ,init-value ,(or doc-string "Undocumented ACT-UP parameter."))
     (push (list ',name ',init-value ,name ,module) *actup-parameters*)))

(defun reset-actup ()
  "Resets architectural ACT-UP parameters, meta-process and current model."
  (loop for (par init-form nil) in *actup-parameters* do
       (set par (eval init-form))))

(defun show-parameters (&optional show-all)
  "Print architectural ACT-UP parameters different from their defaults.
If SHOW-ALL is non-nil, print even unchanged parameters."
  
  (format t ";; ACT-UP parameters:~%")
  (loop for (par nil init-value module) in *actup-parameters* 
       when (not (eq 'internal module))
     do
       (when (or show-all (not (equal init-value (eval par))))
	   (format t (format nil "~~~aT" (max 0 (- 32 (length (symbol-name par))))))
	   (format t "~a ~a ~40T;; (~a)~%" par (eval par) init-value))))

(export '(reset-actup show-parameters))

   
;; Meta processes


(defstruct meta-process
  "An ACT-UP meta process.
A meta process keeps track of time for one or more models."
  (actUP-time 0.0d0 :type  long-float)
  name
)


(setf (documentation 'make-meta-process 'function) "Create a new ACT-UP meta-process.
NAME, if given, specifies a name.
The meta process keeps track of simulation time.
See also `meta-process' and `*current-actup-meta-process*'.")
(setf (documentation 'meta-process-name 'function) "Return the name of an ACT-UP meta-process.
See also `meta-process' and `*current-actup-meta-process*'.")

(def-actup-parameter *current-actup-meta-process* (make-meta-process)
  "The current ACT-UP meta-process.
The meta process keeps track of simulation time.
May be read and manipulated by setting it to a different
instance of type `meta-process'." 'internal)
(export '(meta-process make-meta-process meta-process-name *current-actup-meta-process*))

(defmacro forward-declare (fun args)
  `(DECLAIM (FTYPE (FUNCTION ,(loop for nil in args collect t)
			     t) ,fun)))


  ;; The following helps prevent warnings when loading source code
  ;; but it causes type errors when compiling, as we are not 
  ;; correctly defining the return types.
  ;; (unless (or (fboundp fun)
  ;; 	      (member :lispworks *features*))
  ;; `(defun ,fun ,args
  ;;    (declare (ignore ,@args))
  ;;    (error "forward declaration called.")
  ;;    ;; declare return type:
  ;;    t))
  


;; Debugging

(defvar *critical* 0 "Constant for `*debug*': Show only critical messages.")
(defvar *error* 5 "Constant for `*debug*': Show errors and more important messages.")
(defvar *warning* 10 "Constant for `*debug*': Show warnings and more important messages.")
(defvar *informational* 100 "Constant for `*debug*': Show informational and more important messages.")
(defvar *detailed* 300 "Constant for `*debug*': Show detailed log output .")
(defvar *all* 1000 "Constant for `*debug*': Show all messages (maximum detail).")
(export '(*critical* *warning* *informational* *detailed* *all* *debug*
	  *debug-to-log* debug-log debug-clear debug-detail debug-detail* debug-grep))

(def-actup-parameter *debug* *warning*
  "Level of debug output currently in effect.
The following constants may be used:

*critical* *warning* *informational* *all*

The parameter `*debug-to-log*' is helpful in logging debug messages to a file.")

(def-actup-parameter *debug-to-log* nil
"Enable off-screen logging of debug output.
If t, ACT-UP logs all debug messages not to standard output,
but to a buffer that can be read with `debug-log' and cleared with `debug-clear'.
If a stream, ACT-UP logs to the stream.")

(defvar *debug-stream* nil)


(def-actup-parameter *debug-grep* nil  "Grep debug output with this keyword.")

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

(defun debug-simplify (object)
  (cond ((or (symbolp object) (numberp object) (stringp object))
	 object)
	((actup-chunk-p object)
	 (actup-chunk-name object))
	(t "<S>")))


(defmacro debug-print (level format &rest args)
  `(when (and *debug*
	      (<= ,level *debug*))
     (debug-print-internal ',format ,@args)))
 

(DECLAIM (FTYPE (FUNCTION (stream t) t) pc))

(defun pprint-pc (stream obj)
  (let ((*standard-output* (if (eq stream t) *standard-output* stream)))
    (pc obj)))

(defun debug-print-internal (format &rest args)
  (if *debug-to-log*
    (if (and (not *debug-stream*) (not (streamp *debug-to-log*)))
	(setq *debug-stream* (make-string-output-stream))))
  
  (when format
      (let ((*print-circle* t) (*print-pretty* t)
	    (*print-pprint-dispatch* *print-pprint-dispatch*))
	(set-pprint-dispatch 'actup-chunk #'pprint-pc)	    
	(if *debug-grep*
	    (let ((str (apply #'format nil format args)))
	      (if (loop for g in *debug-grep* do
		       (unless (search g str)
			 (return nil))
		     finally (return t))		    
		  (format (or (if (streamp *debug-to-log*) *debug-to-log* (if *debug-to-log* *debug-stream*)) t)
			  "~a" str)))
	    (apply #'format (or (if (streamp *debug-to-log*) *debug-to-log* (if *debug-to-log* *debug-stream*)) t) format args))
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

(defmacro debug-grep (keyword &body body)
  "Evaluates BODY while outputting ACT-UP debug information."
  `(let  ((*debug* *all*) (*debug-grep* (cons ,keyword *debug-grep*)))
     ,@body))


;;; BASIC MODELS


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro current-model ()
    "Evaluates to the currently active ACT-UP model." 
    '*current-actUP-model*))

;;; PARALEL API

;; this is instantiated for each model and each module

(defvar module-name-list nil)  ; define-module fills this, mkae-model reads it.

(defstruct module
;  (name "mod")
  (name (gensym "MODULE"))  ;; :read-only t)  can't use read-only - does not work if name is given with constructor (why?)
  (last-operation-handle nil)
  (lock nil)
)


(defstruct request-handle 
  module
  result
  busy-until
)

;;(forward-declare current-model ())  ; causes problems in sbcl
(DECLAIM (FTYPE (FUNCTION (model) (VALUES LIST &OPTIONAL)) model-modules))

(defun get-actup-module (symbol)
  (if (request-handle-p symbol)
      (setq symbol (request-handle-module symbol)))
  (let ((module (if (module-p symbol) 
		    symbol
		    (cdr (assoc symbol (model-modules (current-model)))))))
    (unless module
      (error (format nil "get-actup-module: called with unknown module name ~a." symbol)))
    module))


(defmacro with-module-lock (module-name &body body)
  "Execute BODY while locking module"
  ;; (mp:process-wait (format nil "Waiting for ~a module" module-name)
  ;; 		   (lambda ()
  ;; 		     (not (module-lock module))))
  `(let ((module (get-actup-module ,module-name)))
     (setf (module-lock module) 'locked)
     (unwind-protect
	  (progn ,@body)
       (setf (module-lock module) nil))))

(DECLAIM (FTYPE (FUNCTION (&optional meta-process) double-float) actup-time))
(DECLAIM (FTYPE (FUNCTION (number &optional meta-process) t) pass-time))


(defun wait-for-response (handle &optional timeout)
    (if (and handle (request-handle-busy-until handle))
	(let ((remaining (- (request-handle-busy-until handle) (actup-time))))
	  (when (> remaining 0)
	    (if (and timeout (< timeout remaining))
		(pass-time timeout)
		(pass-time remaining))))))

(defun wait-for-module (module-name &optional timeout)
  "Waits until module associated with MODULE-SYM has finished its current operation.
MODUL-SYM may be the name of an ACT-UP module, 
it may be an ACT-UP function belonging to a module,
or it may be a result variable assigned with `request-bind'.

If TIMEOUT is given, never waits longer than that."

  (let* ((module (get-actup-module module-name)))
    (aif (module-last-operation-handle module)
	 (wait-for-response it timeout))))

(defun response-available-p (handle)
  "Returns non-nil if the request associated with HANDLE has terminated.
See `request' functions."
  (if (and handle (request-handle-busy-until handle))
      (>= (actup-time) (request-handle-busy-until handle) )))

(defun module-busy-p (symbol)
  "Determine whether module belonging to object SYMBOL is busy.
SYMBOL may be the name of an ACT-UP module, 
it may be an ACT-UP function belonging to a module,
or it may be a result variable assigned with `request-bind'."
  (let* ((module  (get-actup-module symbol))
	 (handle (module-last-operation-handle module)))
    (not (response-available-p handle))))

(defun reset-module (module-name)
  "Reset module MODULE-NAME.
MODULE-NAME is typically one of `procedural', `declarative'.
Ongoing operations are terminated; their results will 
not become available to the requesters."
  (let ((module (get-actup-module module-name)))
    (let ((handle (module-last-operation-handle module)))
      (when handle
	(setf (request-handle-result handle) nil)
	(setf (request-handle-busy-until handle) 0))
      (setf (module-last-operation-handle module) nil))))

(defun terminate-request (handle)
  "Terminate a parallel request, discarding any results."
  (when handle
    (setf (request-handle-result handle) nil)
    (setf (request-handle-busy-until handle) 0)))
;; however, wouldn't this have already done some "damage"?
;; can we unwind?


(defun define-module (name)
  (push name module-name-list)) ; register


(define-module 'declarative)
(define-module 'procedural)

(defun request (module-name command args)
  "Runs ACT-UP expression (COMMAND . ARGS) asynchronuously."
  (let ((module (get-actup-module module-name)))
    (wait-for-module module)
    ;; eval expression, retain result
    ;; note time, reset time
    (let ((handle (make-request-handle :module module))
	  (time (actup-time)))
      (setf (request-handle-result handle)
	    (apply command args))
      (setf (request-handle-busy-until handle)
	    (setf (request-handle-busy-until handle) (actup-time)))
      (if (and (= time (actup-time)) 
	       ;; some op's take no time when they fail
	       (request-handle-result handle))
	  (debug-print *warning* "~a request intiated (~s), but operation did not take any time."
		       command module-name))
      ;; reset time
      (setf (meta-process-actUP-time *current-actup-meta-process*)
	    time)
      ;; return handle:
      handle)))

(defun receive (handle)
  "Receive result from request HANDLE.
Waits until the result is available.
HANDLE is the handle obtained when the
request was sent via a function such as
`request-retrieve-chunk'."
  (wait-for-response handle)
  (request-handle-result handle))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun filter-argument-list (args)
    (loop for a in args 
       when (or (consp a)
		(and (symbolp a)
		     (not (eq a '&optional))))
       collect
	 (progn
	   (if (eq a '&key)
	       (error "Argument list: &key specifier not supported."))
	   (if (consp a)
	       (car a)
	       a)))))

(defmacro defun-module (module fun-name arglist &body body)
  "Define module function.
MODULE must be a defined ACT-UP module.
FUN-NAME names the function.
ARGLIST contains argument list.  &optional arguments are supported,
but &body is not."
  (let ((req-name (intern (format nil "REQUEST-~a" fun-name)))
	(docstring (if (stringp (car body)) (prog1 (car body) (setq body (cdr body))))))

    `(progn
       (defun ,fun-name ,arglist  
	 ;; we need to wait until this request has been completed:
	 ,docstring
	 (wait-for-module ',module)
	 (with-module-lock ',module
	   ,@body))
       (defun ,req-name (&rest args)
	 ,(format nil "Call `~A' asynchronuously.
Initiates execution of the `~A' function, with ARGS as arguments.

If the ~a module is busy at the current time,
wait until module is free.  The module will be busy and unavailable
for other processing until the current operation has finished.

See also `receive'." fun-name fun-name module)
	 (request ',module ',fun-name args))
       (export ',(list fun-name 
		       ;; req-name   ; not exported until parallelism is tested
		       )))))


;; To do:
;; what happens when expression is more complex, such as
;; (best-chunk (filter-chunks ...)) ?

;; Handling this - when expression may refer to a range of modules -
;; requires multi-threading, since other user threads might 
;; call the different modules at different times and intefere with
;; the results.  Emulating this here would unlikely yield the
;; correct behavior.

;; won't work unless we register the handle with the meta process
;; this much complexity is probably not worth it.
;; (defmacro stuff-received (result-var handle)
;;   (unless (request-handle-p handle)
;;     (signal "stuff-received Hndle must be a request handle from a `request-...' function."))
;;   `(progn
;;      (setf (request-handle-result-closure handle)
;; 	   ;; we use a lexical closure so
;; 	   ;; that result-var is used in the current
;; 	   ;; lexical context.
;; 	   (lambda (value)
;; 	     (setf ,result-var value)))))

;; the following wouldn't work perfectly,
;; because we'd have to do this for all modules in all
;; models that belong to the same meta process (called from pass-time)
;; and we don't know which models these are
;; we also don't have a list of all handles that depend on the
;; the respective timer
;; (defun modules-assign-results ()
;;   (loop for (name . m) in (model-modules (current-model)) 
;;      when (not (module-busy-p m))
;;      when (module-result-closure m) 
;;      do
;;      ;; the closure sets the user-space variable
;;        (funcall (module-result-closure m) (module-result m))
;;        (setf (module-result-closure m) nil)))


 ; not exported until parallelism is tested
;; (export '(request 
;; 	  receive terminate-request response-available-p wait-for-response 
;; 	  module-busy-p wait-for-module reset-module))


;; (print (actup-time))
;; (let ((retrieval-process (request-retrieve-chunk '(:chunk-type person))))
;;   (print (actup-time)) ;; no time has elapsed
;;   (print (response-available-p retrieval-process))  ;; module is busy
;;   (pass-time 0.05) ;; let's spend some time
;;   (print (response-available-p retrieval-process)) ;; module is still busy
;;   ;; (wait-for-response retrieval-process)   ;; wait for result - not needed
;;   ;; (print (response-available-p retrieval-process))
;;   (print (actup-time)) ;; this takes some time!
;;   (print (receive retrieval-process))) ;; waits and receives

;;; MODELS


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
  (indexes `((name . ,(make-hash-table))) :type list)  ; These are indexes.  Currently an alist with one entry:  (name . hash-table)
  (recently-retrieved nil :type list)
  (total-presentations 0 :type integer))

(defstruct procedural-memory
  (regular-procs (make-hash-table))
  (compiled-procs (make-tree))
  (proc-queue nil :type list))


(defstruct model
  (name (gensym "MODEL")) ;; may be used for debugging purposes
  (parms nil :type list)
  ;; overriding model-specific parameters. association list
  ;; of form (PARM . VALUE).
  ;; if an entry for PARM is present, it will be used rather
  ;; than the global binding.
  ;; NOT IMPLEMENTED YET.
  (pm (make-procedural-memory) :type procedural-memory)
  (dm (make-declarative-memory) :type declarative-memory)
  (modules (mapcar (lambda (name) (cons name (make-module :name name)))
		   module-name-list)
	   :type list)  ; alist of module objects
  ;; time (should be in sync with meta-process, unless meta-process is exchanged by user)
  (time 0))

   
(def-actup-parameter *current-actUP-model* (make-model) "Current ACT-UP Model" 'internal)

(setf (documentation 'make-model 'function) "Create a new ACT-UP model.
NAME, if given, specifies a name.")
(setf (documentation 'model-name 'function) "Return the name of an ACT-UP model.")
 
(def-actup-parameter *dat* 0.05 "Default time that it takes to execut an ACT-UP procedure in seconds.
See also: ACT-R parameter :dat  [which pertains to ACT-R productions]")

(defun wait-for-model (&optional model)
  "Waits until meta-process and MODEL are synchronized.
When a model is run with a new meta-process, it can happen that
the meta-process time is behind the model's time (since the model
was operated with a different meta-process before).

This will generate warnings or errors.

This function waits (see `pass-time') until the model is ready, that
is, it sets the meta process time to the model time if the model time
is more advanced, plus the current value of `*dat*'.
MODEL defaults to the current model."
  (let ((diff (- (model-time (or model *current-actUP-model*)) (actup-time))))
    (when (> diff 0.0)
      ;; (format t "~a: waiting for model ~a (t=~a): ~A~%" (meta-process-name *current-actup-meta-process*) 
      ;; 	      (model-name (or model *current-actUP-model*)) (model-time (or model *current-actUP-model*))
      ;; 	      diff)
      (setf (meta-process-actUP-time *current-actup-meta-process*)
	    (+ (meta-process-actUP-time *current-actup-meta-process*) (+ *dat* diff))))))

(defmacro model-chunks (&optional model)
  "Evaluates to the list of chunks in the given model MODEL."
  `(declarative-memory-chunks (model-dm (or ,model *current-actup-model*))))


(defun reset-mp ()
  "Resets the current Meta process. 
Resets the time in the meta process."
  (setq *current-actup-meta-process* (make-meta-process)))

(defun reset-model ()
  "Resets the current ACT-UP model. 
All declarative memory and all subsymbolic knowledge is deleted.
Global parameters (dynamic, global Lisp variables) are retained, as are
functions and model-independent procedures."
  (setq *current-actUP-model* (make-model)))



;; DECLARATIVE MODULE  / CHUNKS


;; parameters

(def-actup-parameter *bll* 0.5 "Base-level learning decay parameter for declarative memory.
See also: ACT-R parameter :bll")
(def-actup-parameter *blc* 0.0 "Base-level constant parameter for declarative memory.
See also: ACT-R parameter :blc") 
(def-actup-parameter *rt* 0.0 "Retrieval Threshold parameter for declarative memory.
Chunks with activation lower than `*rt*' are not retrieved.
See also: ACT-R parameter :rt")  ; can be (cons 'pres 4)

(def-actup-parameter *ans* 0.2 "Transient noise parameter for declarative memory.
See also: ACT-R parameter :ans") ;; transient noise  

(def-actup-parameter *pas* nil "Permanent noise parameter for declarative memory.
See also: ACT-R parameter :pas") ;; permanent noise  

(export '(*bll* *blc* *rt* *ans* *pas* *dat*))


(defvar *ol* 3  "Optimized Learning parameter for base-level learning in Declarative Memory.
OL is always on in ACT-UP.
See also: ACT-R parameter :ol")

(def-actup-parameter *associative-learning* nil ; would be 1.0 if on
  "The trigger for associative learning, a in ROM Equation 4.5.
   Can be any non-negative value.")
(export '(*ol* *associative-learning*))


(defun actup-noise (s)
  (if s
      (act-r-noise s)
      0.0))


;; all chunks inherit from this structure:


(export '(actup-chunk define-chunk-type 
	  ;; public members of actup-chunk:
	  ;;name 
	  chunk-type))

(defvar *actup-chunk-count* 0)

(defun gensym-chunk ()
  (intern (format nil "CHUNK-~a" (incf *actup-chunk-count*))))

(eval-when (:compile-toplevel :load-toplevel :execute) 
  ;; structure should be available at compile time
(defstruct actup-chunk
  "Type defining an ACT-UP chunk.
Derive your own chunks using this as a base structure
by using `define-chunk'."
  ;; helpful for debugging
  (name (gensym-chunk) :read-only t)
  (comment) ;; documentation / comments
  (chunk-type nil)
  (attrs nil)  ;; list of user-defined slots

  ;; internal ACT-UP structures
  (total-presentations 0 :type integer)
  (first-presentation (actup-time))
  (recent-presentations nil :type list) ; with the most recent one in car!
  (last-bl-activation 0)
  (activation-time nil)

  ;; we guarantee that the noise is constant
  ;; if time is constant
  (last-noise nil)
  (last-noise-time nil)
  (permanent-noise (actup-noise *pas*) :type float)
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
  (model nil)  ; pointer to the chunk's model  (to ensure it's unique)
))

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
	 

(defmacro normalize-slotname (slot)
  `(intern (string-upcase (symbol-name ,slot))))
(defmacro normalize-slotname-with-package (slot)
  `(intern (string-upcase (symbol-name ,slot)) 'act-up))

(defun chunk-name (chunk)
  "The unique name of CHUNK.
The returned value is a symbol assigned as unique name of CHUNK
in the current model."
  (actup-chunk-name chunk))

(defun get-chunk-name (chunk-or-name)
  (if (actup-chunk-p chunk-or-name)
      (actup-chunk-name chunk-or-name)
      chunk-or-name))

;; To Do:  use hash to speed this up 
(defun get-chunk-by-name (name)
  "Returns first chunks of name NAME"
		
  (let ((index (cdr (assoc 'name (declarative-memory-indexes (model-dm (current-model)))))))
    (if index
	(car (gethash name index))  ; there should only be one chunk of any given name, so we do `car'
	;; no index available, for whatever reason
	(loop for c in (model-chunks *current-actUP-model*)
	  do
	     (if (equal name (actup-chunk-name c))
		 (return c))))))
       
(defun get-chunk-object (chunk-or-name &optional noerror)
  "Returns chunk object for CHUNK-OR-NAME.
If CHUNK-OR-NAME is a chunk, return is.
Otherwise, returns chunk by name CHUNK-OR-NAME
from current model DM.
Returns nil if NOERROR is non-nil, otherwise signals an error if chunk can't be found."
  (if (actup-chunk-p chunk-or-name)
      chunk-or-name
      (or (get-chunk-by-name chunk-or-name)
	  (if noerror
	      nil
	      (error (format-nil "Chunk of name ~a not found in DM." chunk-or-name))))))


(defun get-chunk-objects (chunks-or-names &optional noerror)
  (loop for c in chunks-or-names append
       (let ((co (get-chunk-object c noerror)))
	 (if co (list co) nil))))

(defun chunk-slot (chunk slot-name)
 (slot-value chunk slot-name))
 

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

(def-actup-parameter *maximum-associative-strength* 1.0 "Maximum associative strength parameter for Declarative Memory.
`*mas*' is defined as alias for `maximum-associative-strength'.
See also `*associative-learning*', `reset-sji-fct'.
See also: ACT-R parameter :mas.")
(define-symbol-macro *mas* *maximum-associative-strength*) ; compatibility macro
(export '(*maximum-associative-strength* *mas*))

(defmacro count-occ-ji (val chunk)
  `(count-if (lambda (slot)
	       (let ((sv (slot-value ,chunk slot)))
		 (eq ,val (if (actup-chunk-p sv) (actup-chunk-name sv) sv))))
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
    ;; (format-t "fan-ji: c:~s n:~s  occ:~s  occs: ~s~%" c n occ (count-occurrences c))
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
    

;; ACTIVATION CALCULATION

(defun bll-sim (pres-count lifetime)
  (+ *blc* (log (/ pres-count (- 1 *bll*))) (- (* *bll* (log lifetime)))))

(defun actup-chunk-get-base-level-activation (chunk)

  ;; we're using the Optimized Learning function

  ;; This assumes that at least *dat* time has passed since the initial presentation.
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
	   (loop for pres in (actup-chunk-recent-presentations chunk) 
	      sum
		(let ((decay-time  (- time pres)))
		  ;; (format-t "~a: adding ~a s, ~a~%" (actup-chunk-name chunk) (- time pres) decay)
		  (when (< decay-time *dat*)
		      (debug-print *warning* "~a: Warning: Base-level activation of chunk ~a measured too shortly (~2,2f-~2,2f<~asec) after its latest presentation (learn-chunk). Model-time: ~a:~2,2f.~%"
				   (meta-process-name *current-actup-meta-process*)
				   (actup-chunk-name chunk)
				   time pres *dat* (model-name *current-actUP-model*) (model-time *current-actUP-model*))
		      (print chunk)
		      (error 'warning)
		      )	   
		  (expt (max *dat* decay-time) (- *bll*))))
	   
	   (let ((k (length (actup-chunk-recent-presentations chunk))))
	     (if (and (> (actup-chunk-total-presentations chunk) k) (actup-chunk-first-presentation chunk))
		 ;; optimized learning
		 (let ((last-pres-time (max *dat* (- time (or (car (last (actup-chunk-recent-presentations chunk))) 
							  (actup-chunk-first-presentation chunk))))) ;; 0? ;; tn
		       (first-pres-time (max *dat* (- time (actup-chunk-first-presentation chunk)))))
		   (if (and first-pres-time
			    (not (= first-pres-time last-pres-time)))
		       (progn
			 (/ (* (- (actup-chunk-total-presentations chunk) k) 
			       (max 0.1 (- (expt first-pres-time 1-d) (expt last-pres-time 1-d))))
			    (* 1-d (max *dat* (- first-pres-time last-pres-time)))))
		       0.0))
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
		   
		 0.0))))) ; !!!
       0.0)
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
			 (+ (or (if link (actup-link-sji link)) 0) ; add on Sji (is this the right thing to do?)
			    (let ((rji (chunk-get-rji cue chunk)))  (log-safe rji)))
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


(def-actup-parameter *mp* 1.0 "ACT-UP Partial Match Scaling parameter
Mismatch (`set-similarities-fct') is linearly scaled using this coefficient.")

(def-actup-parameter *ms* 0 "ACT-UP Partial Match Maximum Similarity
Similarity penalty assigned when chunks are equal.
Value in activation (log) space.")

(def-actup-parameter *md* -1 "ACT-UP Partial Match Maximum Difference
Similarity penalty assigned when chunks are different
and no explicit similarity is set.
Value in activation (log) space.")

(export '(*mp* *ms* *md*))

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


(defun actup-chunk-get-partial-match-score (chunk retrieval-spec)
  (if *mp*
      (progn ; (print retrieval-spec)

	(* *mp*
	   (loop for (s v) on retrieval-spec  by #'cddr sum
		(value-get-similarity (slot-value chunk (normalize-slotname s)) v))
	   ))

      ;; else
      0))


(defun actup-chunk-get-noise (chunk)
  (+  (if *ans* 
	  (or (and (eq (actUP-time) (actup-chunk-last-noise-time chunk))
		   (actup-chunk-last-noise chunk))
	      (progn
		(setf (actup-chunk-last-noise chunk) (actup-noise *ans*)
		      (actup-chunk-last-noise-time chunk) (actUP-time))
		(actup-chunk-last-noise chunk)))
	  0)
      (if *pas*
	  (actup-chunk-permanent-noise chunk)
	  0)))

(defun actup-chunk-get-activation (chunk &optional cue-chunks retrieval-spec)
  "Calculate current activation of chunk"

  (let ((base-level (actup-chunk-get-base-level-activation chunk))
	(spreading (actup-chunk-get-spreading-activation chunk cue-chunks))
	(partial-matching (actup-chunk-get-partial-match-score chunk retrieval-spec))
	(noise (actup-chunk-get-noise chunk)))
	
	(+ base-level spreading partial-matching noise)))


(defun explain-activation (chunk-or-name &optional cues retr-spec)
  "Returns a string with an explanation of the evaluation of CHUNK.
CUES contains retrieval cues spreading activation.
RETR-SPEC describes the retrieval specification for partial matching retrievals."
  (when chunk-or-name
    (let ((chunk (get-chunk-object chunk-or-name)))
    (format-nil "  time: ~2,2f  ~a base-level: ~2,2f  (~a pres) pm: ~2,2f ~2,2f ~2,2f"
	    (actUP-time)
	    (actup-chunk-name chunk)
	    (actup-chunk-get-base-level-activation chunk)
	    (actup-chunk-total-presentations chunk) ;; (actup-chunk-recent-presentations chunk)
	    (if cues
		(format-nil "  spreading: ~a~%     ~a" (actup-chunk-get-spreading-activation chunk (get-chunk-objects cues 'noerror))
			;; explain
			(when (>= *debug* *all*)
			  (loop for cue in (get-chunk-objects cues) 
			     for link = (cdr (assoc (get-chunk-name chunk) (actup-chunk-related-chunks (get-chunk-object cue))))
			     
			     collect
			       (format-nil "~a: ~a" (get-chunk-name cue)
				  (if  (and link (actup-link-sji link))
				       (format-nil "Sji: ~2,2f " (actup-link-sji link))
				       (format-nil "Rji: ~2,2f " (let ((rji (chunk-get-rji cue chunk))) (log-safe rji))))))))
		"")
	    (if retr-spec 
		(format-nil "partial match: ~a " (actup-chunk-get-partial-match-score chunk retr-spec))
		"-")
	    (if *ans* (format-nil "tr.noise: ~a " (actup-chunk-last-noise chunk)) "-")
	    ))))


(defun make-chunk (&rest args)
  "Create an ACT-UP chunk.
Arguments should consist of named chunk feature values: ARGS is a list
of the form (:name1 val1 :name2 val2 ...), whereas names correspond to
slot names as defined with `define-slots'.  

An attribute called `:name' should be included to specify the unique
name of the chunk (the name may not be used for any other chunk in
the model). 

If chunk types are defined with `define-chunk-type', then use the
`make-TYPE' syntax instead."

  (apply #'make-actup-chunk args))


(DECLAIM (FTYPE (FUNCTION (t &rest t) actup-chunk) make-match-chunk))

(defun make-chunk* (&rest args)
 "Like `make-chunk', but returns matching chunk from declarative memory if one exists.

Arguments should consist of named chunk feature values: ARGS is a list
of the form (:name1 val1 :name2 val2 ...), whereas names correspond to
slot names as defined with `define-slots'.  

An attribute called `:name' should be included to specify the 
name of the chunk. Comparing the proposed chunks (in ARGS) to the existing
chunks in Declarative Memory, the names of the chunks are ignored.

The purpose of this function lies in the ability to boost a chunk existing in DM, when its contents are already known. For example:

 (reset-model)
 (learn-chunk (make-chunk* :one 1 :two 2))
 (learn-chunk (make-chunk* :one 1 :two 2))

will create a chunk (first call), and then boost it, while

 (learn-chunk (make-chunk :one 1 :two 2))

will always create new chunk and add it to declarative memory.

If chunk types are defined with `define-chunk-type', then use the
`make-TYPE*' syntax instead."

 (apply #'make-match-chunk 'actup-chunk args))



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
in the model). 

Chunk contents must not be changed after a chunk has been created.

An additional function of name make-TYPE* is also provided, which
creates a new chunk just like make-TYPE does, but only if such a chunk
does not yet exist in declarative memory (of the current model). All
slot values of the chunks are used in the comparison (unspecified ones
at their default values), except the :name attribute.  If a matching
chunk is found in DM, it is returned."
  
  (let* ((name-and-options type)
	 (type (if (consp type) (car type) type))
	 (attr-list (loop for m in members collect
			 (if (consp m)
			     (car m) m)))
	 (incl
	  (if (consp name-and-options)
	      (list (car name-and-options)
		    (if (eq (cadr name-and-options) :include)
			`(:include ,(caddr name-and-options))
			(error "define-chunk-type: faulty options in NAME.")))
	      (list name-and-options `(:include actup-chunk
				       (chunk-type ',type)
				       (attrs ',attr-list))))))
    ;; mark each member as read-only
    (setq members 
	  (loop for m in members collect
	       (append
		(if (consp m) 
		    (if (cdr m) m (list (car m))) (list m nil))
		'(:read-only t))))
    `(progn
       (defstruct ,incl
	 ,@members)
       ;; define special constructor
       (defun ,(intern (format nil "MAKE-~a*" type)) (&rest args)
	 ,(format nil 
 "Like make-~a, but returns matching chunk from declarative memory if one exists."
		  type)
	 (apply #'act-up::make-match-chunk ',type args))
       )))
 

(defmacro define-slots (&rest slot-names)
  "Define slots to be used in chunks of this process.
Only slot names defined using this macro may be used in chunks.
Overrides any slot set defined earlier."
  `(define-chunk-type chunk ,@slot-names))

(export '(pc pc*))
(defun pc (obj &key (stream t) (internals nil)) 
  "Print a human-readable representation of chunk OBJ.
STREAM, if given, indicates the stream to which output is sent.
INTERNALS, if given and t, causes `pc' to print architectural
internals (see also `pc*' for a shortcut)."
  (loop for obj in (if (listp obj) obj (list obj)) do
  (let ((obj (get-chunk-object obj))
	(*print-circle* t)
	(*print-level* 3)
	(stream (or stream t)))
    (handler-case
     (progn
       (format stream "~a~%" (actup-chunk-name obj))
       (loop for slot in (append  '(chunk-type) (if (actup-chunk-comment obj) '(comment)) (actup-chunk-attrs obj)
				 (if internals '(total-presentations first-presentation recent-resentations related-chunks references similiar-chunks model)))
	  for val = (safe-slot-value obj slot)

	  do
	    (format stream (format nil "~~~aT" (max 0 (- 23 (length (symbol-name slot))))))
	    (format stream "~a: ~a ~%"  slot (cond
	      ((listp val)
	       (loop for i from 1 to 6
		    for v in val collect
		    (if (= i 6)
			'+...+
			(get-chunk-name v))))
	      ((floatp val)
	       (format nil "~2,4f" val))
	      ((model-p val)
	       (format nil "~2,4f" (model-name val)))
	      (t (get-chunk-name val)))))
       (format stream "~%")

       ;; (if (actup-chunk-related-chunks obj)
       ;; 	   (format stream "related chunks: ~a~%" (mapcar (lambda (x) (if (actup-chunk-p x) (actup-chunk-name x) x)) (actup-chunk-related-chunks obj))))
       (format stream "~%"))
     (error (v) (progn (format stream "ERR~a" v) nil))))))

(defun pc* (obj &key (stream t))
  "Print a human-readable representation of chunk OBJ, including architectural internals.
STREAM, if given, indicates the stream to which output is sent."
  (pc obj :stream stream :internals t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIENT FUNCTIONS

(export '(current-model set-current-model with-current-model 
	  make-model model-name
	  actUP-time stop-actup-time
	  pass-time wait-for-model
	  model-chunks 
	  defproc defrule assign-reward assign-reward* flush-procedure-queue
	  define-slots define-chunk-type
	  make-chunk ; for untyped chunks
	  make-chunk* ; for untyped chunks
	  show-chunks chunk-name explain-activation
	  non-nil
	  reset-mp reset-model
	  reset-sji-fct set-similarities-fct add-sji-fct set-dm-total-presentations set-base-level-fct set-base-levels-fct))

(export '(retrieve-chunk blend-retrieve-chunk
	  filter-chunks add-chunk-to-dm learn-chunk best-chunk blend))

(defun current-actUP-model ()
  "Evaluates to the currently active ACT-UP model."
  (or *current-actUP-model*
      (error "No model active.")))

(defun set-current-model (new-model)
  "Switches the currently active ACT-UP model.
See also `current-model' and `with-current-model'."
  (setq *current-actUP-model* new-model)
  (model-name new-model))

(defmacro with-current-model (model &body body)
  "Execute forms in BODY with the ACT-UP model MODEL being current.
See also `current-model' and `set-current-model'."
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

  `(let* ((mp *current-actup-meta-process*)
	  (actup---actup-time-t0 (meta-process-actUP-time mp)))
     ,@body
     (- (meta-process-actUP-time mp) actup---actup-time-t0)))


(defun actup-time (&optional meta-process)
  "Returns the current runtime.
An optional parameter META-PROCESS specifies the meta-process to use.
It defaults to the current meta-process."
  (meta-process-actUP-time (or meta-process *current-actup-meta-process*)))

(defvar *actup-timeout* nil)
(defun pass-time (seconds &optional meta-process)
  "Simulates the passing of time.
An optional parameter META-PROCESS specifies the meta-process to use.
It defaults to the current meta-process."
  (when (> seconds 0.0)
    (setf (meta-process-actUP-time (or meta-process *current-actup-meta-process*))
	  (+ (meta-process-actUP-time (or meta-process *current-actup-meta-process*)) seconds))
    (setf (model-time *current-actUP-model*)
	  (meta-process-actUP-time (or meta-process *current-actup-meta-process*))))
  ;; (format t "~a: ~a ~a ~%"
  ;; 	  (meta-process-name *current-actup-meta-process*) 
  ;; 	  (model-time *current-actUP-model*) (meta-process-actUP-time (or meta-process *current-actup-meta-process*)))
  (and *actup-timeout*
       (> (model-time *current-actUP-model*)
	  *actup-timeout*)
       
       (throw 'actup-timeout 'timeout)))

(defmacro with-actup-timeout (timeout &body body)
  `(let ((*actup-timeout* (+ ,timeout (meta-process-actUP-time *current-actup-meta-process*))))
     (let ((result
	    (catch 'actup-timeout
	      ,@body)))
       ;; (if (eq result 'timeout)
       ;; 	   (debug-print *warning* "with-actup-time: timeout reached after ~2,2f sec.." ,timeout) t)
       result)))



(def-actup-parameter *lf* 1.0 "Latency Factor parameter for declarative retrieval time calculation.
See ACT-R parameter :lf")
(def-actup-parameter *le* 1.0  "Latency Exponent parameter for declarative retrieval time calculation.
See ACT-R parameter :le")
(export '(*lf* *le*))

(def-actup-parameter *declarative-num-finsts* 4  "Number of Declarative Finsts

The maximum number of chunks considered recently retrieved.

Defaults to 4.

See ACT-R parameter :declarative-num-finsts")
(def-actup-parameter *declarative-finst-span* 3.0  "Declarative Finst time span

The maximum time period during whichg a finst marks a chunk as recently retrieved.
Chunks retrieved longer ago are not considered 'recently retrieved'.

Time in seconds, defaults to 3.0.

See ACT-R parameter :declarative-finst-span")
(export '(*declarative-num-finsts* *declarative-finst-span*))


(defun-module declarative best-chunk (confusion-set &key cues pm-soft-spec timeout inhibit-cues)
"Retrieves the best chunk in confusion set.
CONFUSION-SET is a list of chunks, out of which the chunk is returned.
CUES is a list of cues that spread activation.  CUES may contain
chunk objects or names of chunks.
PM-SOFT-SPEC: request specification for partial matching (see also `retrieve-chunk').
INHIBIT-CUES: do not use (yet).

Simulates timing behavior with `pass-time'.

Marks the chunk as recently retrieved (declarative finst).

Note that this function extends beyond the power of ACT-R's
declarative module.

See also the higher-level function `retrieve-chunk'."

 ;; retrieve using spreading activation from cues to confusion-set
;; must go through this even for empty confusion set
;; because we need to pass-time in this case
      (let* ((last-retrieved-activation nil)
	     (cues (get-chunk-objects cues))
	     (below-rt-count 0)
	     (best  (loop  with bc = nil with bs = nil 
		       for c in confusion-set
		       when (or (not inhibit-cues) (not (member c cues)))
		       when c ;; allow nil chunks
		       do
			 (let ((s (actup-chunk-get-activation c cues pm-soft-spec)))
			   (if (or (not *rt*) 
				   (if (consp *rt*)
				       (> (actup-chunk-total-presentations c) (cdr *rt*))
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
		(when best
		  ;; mark as retrieved 
		  (push (cons (actup-chunk-name best) (actup-time))  (declarative-memory-recently-retrieved (model-dm (current-model))))
		  ;; limit number of chunks
		  (awhen (nthcdr *declarative-num-finsts* (declarative-memory-recently-retrieved (model-dm (current-model))))
		    (setf (cdr it) nil)))
		best)))))

(defun-module declarative filter-chunks (chunk-set spec &key (recently-retrieved 'ignore))
  "Filter chunks according to SPEC.
SPEC is a list of the form (:slot1 value1 :slot2 value2 ...),
or (slot1 value1 slot2 value2).
CHUNK-SET is the list of chunks to be filtered (1), or an associative array (2)
of the form ((X . chunk1) (Y . chunk2) ...).
returns a list of chunks in case (1) and a list of conses in case (2)."

  ;(let ((csn nil))
  (let* ((rr (declarative-memory-recently-retrieved (model-dm (current-model))))
	 (matching-chunks 
	 (loop for chunk-or-cons in chunk-set append
	      (let ((c (if (consp chunk-or-cons) (cdr chunk-or-cons) chunk-or-cons)))
		(if (or (eq recently-retrieved 'ignore)
			(if (eq recently-retrieved t)
			    ;; check that we're not beyond the time span
			    (let ((mb (assoc (actup-chunk-name c) rr)))
			      (if mb
				  (> (cdr mb) (- (actup-time) *declarative-finst-span*))))
			    ;; else 
			    (if (eq recently-retrieved nil)
				(not (assoc (actup-chunk-name c) rr)))))
		  
		(handler-case
		    (if (loop for argval on spec by #'cddr finally (return t) do
			     (let* ((slot ;(setq csn  
				     (normalize-slotname (first argval))) ;)
				    (vv (second argval))
				    (slot-value (slot-value c slot)))
			       ;; should nt make a difference:
			       ;; (if (actup-chunk-p vv) (setq vv (actup-chunk-name vv)))
			       ;; (if (actup-chunk-p slot-value) (setq slot-value (actup-chunk-name slot-value)))
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
		    )))))))
    (debug-print  *informational* "filtered ~a matching chunks.~%" (length matching-chunks))
    matching-chunks))
; (macroexpand '(defun-module declarative retrieve-chunk (spec &optional cues pm-soft-spec timeout)))




;; (defmethod slot-missing (class (object objc) slot-name 
;; 			 (operation (eql 'slot-value)) &optional new-value) 

;;   object slot-name operation new-value;; ignore
;;   'missing)

;; (normalize-slotname-with-package :name 'act-up)


(defun search-for-chunks (model args)
;  (say "searching for chunks ~a" args)
  (filter-chunks (model-chunks model) args))



(defun make-struct-instance (type &rest args)
  ;; MAKE-INSTANCE is not guaranteed for `defstruct'-defined types
  ;; (it is meant to CLASS types only)
  (apply (intern (format-nil "MAKE-~a" type))
	 args))

;; for each chunk type, we define
(defun make-match-chunk (type &rest chunk-spec)
  "Make a chunk of TYPE or return matching chunk from DM.
If a matching chunk is found in DM, return it.
Otherwise, call `make-TYPE' constructor with CHUNK-SPEC to make a new chunk.
This new chunk is not added to DM.

For purposes of matching, the values of each slot are compared with the
chunk created from TYPE and CHUNK-SPEC; any slots not specified
in CHUNK-SPEC assume the default value (often nil) for purposes of
comparison.

CHUNK-SPEC may contain a single element, which must be a chunk.  In that case,
this chunk is used for the comparison (and returned if no matching chunk is found.)

Note that chunk type (see `define-chunk-type') also has a constructor
function of name `make-TYPE*', taking just the CHUNK-SPEC argument,
which calls this function."

  (let ((template (if (and (not (cdr chunk-spec)) (actup-chunk-p (car chunk-spec)))
		      (car chunk-spec)
		      (apply #'make-struct-instance type chunk-spec)))
	(filtered-chunks (model-chunks (current-model))))
    (loop for slot in (cons 'act-up::chunk-type (slot-value template 'act-up::attrs)) ; this should exclude :name

       for value = (slot-value template slot)
       when (not (eq slot :name))
       do
	 (setq filtered-chunks
	       (loop for chunk in filtered-chunks append
					; chunk type is filtered first
		  ;; so we should have a guarantee that all slots are present
		    (if (equal (slot-value chunk slot) value)
			(list chunk))
		    )))

    ;;
    (if filtered-chunks
	(progn
	  (if (> (length filtered-chunks) 1)
	      (debug-print *warning* "make-match-chunk (make-TYPE*): ~a matching chunks found in DM.  Returning first.~%" (length filtered-chunks))
	      (debug-print *informational* "make-match-chunk (make-TYPE*): Found matching chunk of name ~a~%" (actup-chunk-name (car filtered-chunks))))
	  (car filtered-chunks))
	(progn
	  ;; check that we don't have a chunk of the same name in DM
	  (let ((name (actup-chunk-name template)))
	    (when (get-chunk-by-name name)
	      (debug-print *informational* "New chunk:~a~%Existing chunk in DM:~a~%" template (get-chunk-by-name name))
	      (error (format nil "make-match-chunk (make-TYPE*): Novel chunk with name ~a to be created, different chunk of same name already in DM."
			     name))))

	  (debug-print *informational* "make-match-chunk (make-TYPE*): No such chunk in DM.  Returning new chunk (not in DM) of name ~a~%" (actup-chunk-name template))
	  template))))


(DECLAIM (FTYPE (FUNCTION (t &optional t) t) learn-chunk))

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



(defun add-chunk-to-dm (chunk first-presentation-time recent-presentation-times number-of-presentations)
  "Add CHUNK to declarative memory of current model.
FIRST-PRESENTATION-TIME indicates the time of first presentation of the chunk (see also `actup-time').
RECENT-PRESENTATION-TIMES is a list of the `*ol*' or less most recent presentation times.
NUMBER-OF-PRESENTATIONS indicates the total number of presentation, including the first one."

	    ;;(not (member chunk (model-chunks model))) ;; use object in DM if present
	    (let ((model (current-model)))
	      ;; make sure the chunk isn't assigned to a different model
	      (if (actup-chunk-model chunk)
		  (if (not (eq (actup-chunk-model chunk) (model-name model)))
		      (error (format-nil "learn-chunk: chunk belonging to model ~a being added to model ~a." (model-name (actup-chunk-model chunk)) (model-name model))))
		  ;; else: not set
		  (setf (actup-chunk-model chunk) (model-name model)))
	      ;; make sure we don't have a chunk of the same name already present
	      (let ((new-name (get-chunk-name chunk))
		    (existing-chunk))
		(and new-name
		     (setq existing-chunk (get-chunk-by-name new-name))
		     (not (equal existing-chunk chunk))
		     (progn (debug-print *informational*
				  "Existing chunk: ~a ~%New chunk: ~a~%"
				  existing-chunk
				  chunk)
			    (error (format-nil "learn-chunk/add-chunk-to-dm: Another chunk of name ~s is already in DM when trying to learn chunk ~s." new-name chunk))))

		(unless existing-chunk  ;; should only happen if chunk is `equal' to an existing chunk
		  ;; upon adding, always reset first presentation time
		  (setf (actup-chunk-first-presentation chunk) first-presentation-time)
		  (setf (actup-chunk-recent-presentations chunk) recent-presentation-times)
		  (setf (actup-chunk-total-presentations chunk) number-of-presentations)
		  (push chunk (model-chunks model))
		  ;; this is the only location where we're manipulating model-chunks.
		  ;; update index:
		  
		  (let ((index (cdr (assoc 'name (declarative-memory-indexes (model-dm (current-model)))))))
		    (push chunk (gethash new-name index)))))))

;; (defparameter *actup--chunk-slots* (mapcar #'car (structure-alist (make-chunk)))
;;   "Internal to ACT-UP.")
(defun learn-chunk (chunk &optional co-presentations)
  "Learn chunk CHUNK.

This will note a presentation of a chunk in the model's DM.
If the chunk does not already exist in DM, it is added.

To create or obtain the chunk from a attribute-value specification,
use `make-chunk' and `make-chunk*' (or their corresponding constructor 
functions for a specific chunk type - see `define-chunk-type'), then
apply `learn-chunk' on the result.

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
	(if (let ((existing-chunk (get-chunk-by-name (get-chunk-name chunk))))
	      (or (not existing-chunk) ; not chunk of such name
		  (not (eq existing-chunk chunk))))
	    (add-chunk-to-dm chunk (actup-time) nil 0))
	     ; else: already a member.
	; else: get from DM by name
	; maybe learn implicitly?
	(setq chunk (get-chunk-by-name chunk)))   ;; retrieve by name
		      
    ;; (car (search-for-chunks model chunk-descr)) ;; use unifiable object 
    
    (debug-print *informational* "Presentation of chunk ~a.~%"  ;;  (MP: ~a t=~a.  M: ~a, t=~a
		 (actup-chunk-name chunk) 
		 ;; (meta-process-name *current-actup-meta-process*)
		 ;; (actup-time)
		 ;; (model-name model)
		 ;; (model-time model)
		 )

    (incf (actup-chunk-total-presentations chunk))
    
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
	 when val
       when (or (actup-chunk-p val) (symbolp val))
       do
	 (let ((ca (get-chunk-object-add-to-dm val)))
	   (when ca
	     (pushnew name (actup-chunk-occurs-in ca)))))

    (push (actUP-time) (actup-chunk-recent-presentations chunk))
    (awhen (nthcdr (1- *ol*) (actup-chunk-recent-presentations chunk))
      ;;    (if (> (length (actup-chunk-recent-presentations chunk)) *ol*)
	(setf (cdr it) nil))

    ;; copy actup-chunk information
    ;; (when (actup-chunk-p chunk)
    ;;   (loop for slot1 in *actup--chunk-slots*
    ;; 	 for slot = (normalize-slotname-with-package slot1)
    ;; 	 do
    ;; 	   (setf (slot-value chunk slot) (slot-value chunk slot))))
    
    (pass-time *dat*) ;; 50ms
    chunk))

(defun chunk-name-not-unique (name)
  "Returns non-nil if more than one chunk of name NAME exists.
Returns nil if name is nil."
  (unless (null name)
    (let ((num (loop for c in (model-chunks *current-actUP-model*)
		  when (equal name (actup-chunk-name c))
		  sum 1)))
      (if (> num 1)
	  num))))
       

(defun best-n-chunks (n confusion-set &key cues pm-soft-spec)
  "Retrieves the best chunks in confusion set.
CONFUSION-SET is a list of chunks, out of which the best N chunks will
be returned. CUES is a list of cues that spread activation.  CUES may
contain chunk objects or names of chunks.
PM-SOFT-SPEC: request specification for partial matching (see also `retrieve-chunk').

Does not simulate timing behavior, and does not mark the chunk as
recently retrieved.  Note that this function extends beyond the power
of ACT-R's declarative module.  

See also the higher-level functions `retrieve-chunk' and
`blend-retrieve-chunk', and the lower-level `best-chunk' function."

  ;; retrieve using spreading activation from cues to confusion-set
  (if confusion-set
      (let* ((cues (get-chunk-objects cues))
	     (all  (loop for c in confusion-set 
		  append
		    (let ((s (actup-chunk-get-activation c cues pm-soft-spec)))
		     
		      (if (or (not *rt*) 
			      (if (consp *rt*)
				  (> (actup-chunk-total-presentations c) (cdr *rt*))
				  (> s *rt*)))
			  (list (cons s c)))))
			  ))
	(mapcar 'cdr (subseq (stable-sort all #'> :key #'car) 0 (min n (length all) ))))))

 
(defun-module declarative retrieve-chunk (spec &optional cues pm-soft-spec timeout &key (recently-retrieved 'ignore))
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
the retrieval fails.

RECENTLY-RETRIEVED, if given, may be either `t', in which case
the retrieved chunk must have a declarative finst (i.e., has been
recently retrieved), or `nil', in which is must not have a finst.
See also `*declarative-num-finsts*' and `*declarative-finst-span*'."
  (debug-print *informational* "retrieve-chunk:~%   spec: ~a~%  cues: ~a~%  pmat: ~a~%" spec cues pm-soft-spec)
  (let* ((matching-chunks (let ((*debug* (max *debug* *warning*)))
			    (filter-chunks (model-chunks *current-actUP-model*)
					   spec :recently-retrieved recently-retrieved)))
	 (best-chunk  (let ((*debug* (max *debug* *warning*)))
			(best-chunk matching-chunks
				    :cues cues :pm-soft-spec pm-soft-spec :timeout timeout))))
    (debug-print  *informational* "retrieved ~a out of ~a matching chunks.~%" (if best-chunk (or (actup-chunk-name best-chunk) "one") "none") (length matching-chunks))
    (debug-print *informational* "~a~%" (explain-activation best-chunk cues pm-soft-spec))
    ;; to do: add if to make fast
    (loop for c in matching-chunks do 
	 (debug-print *detailed* "~a~%" (explain-activation c cues (append spec pm-soft-spec))))
    best-chunk))
 
(def-actup-parameter *blend-temperature* 1.0 "Blend temperature.")

(defun blend (chunks &keys cues chunk-type retrieval-spec)
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
	       (error (format nil
			      "blend: Found chunk of different types ~a and ~a, and no CHUNK-TYPE given."
			      (class-name (class-of c)) (class-name chunk-type))))
	     (if chunk-type 
		 (unless (subtypep chunk-type (class-of c))
		   (error (format nil "blend: Chunk of type ~a is not supertype of given type ~a."
				  (class-name (class-of c)) (class-name chunk-type))))
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
	    (apply (find-symbol (format-nil "MAKE-~a" (class-name chunk-type)))
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



(defun blend-retrieve-chunk (spec &key cues pm-soft-spec (recently-retrieved 'ignore))
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
			     spec :recently-retrieved recently-retrieved)))
    (if cs
	(blend cs cues nil (append spec pm-soft-spec)))))


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

(forward-declare search-for-chunks (model constraints))

(defun show-chunks (&optional constraints)
  "Prints all chunks in model MODEL subject to CONSTRAINTS.
See the function `filter-chunks' for a description of possible constraints."
  (print (mapcar 'chunk-name (if constraints
				 (search-for-chunks model constraints)
				 (declarative-memory-chunks (model-dm (current-model))))))
  nil)



 

;; export

    

; to do
; relax "member" - can't work like this
; update activation
; update references
; initialize name if necessary
; 


   ;; set time


;   (setf (declarative-memory-chunks (model-dm (current-model))) chunks)

;; PROCEDURAL


;; this is, as of now, independent of the model
(defparameter *actup-procgroups* nil)

(def-actup-parameter *au-rpps* nil
  "Reward proportion per second elapsed.
e.g., after 10 seconds we want to assign 50% of the remaining reward: 0.5/10 = 0.05
time is in between procedures.
Set to nil (default) to use the ACT-R discounting by time in seconds.
See also the parameter `*au-rfr*' and the function `assign-reward'.")

(def-actup-parameter *au-rfr* nil
  "base reward proportion for each procedure
e.g., the each procedure before the reward trigger gets 10% of the reward.
Set to nil (default) to use the ACT-R discounting by time in seconds.
See also the parameter `*au-rpps*' and the function `assign-reward'.")
	   
(def-actup-parameter *alpha* 0.2  "Utility learning rate.
See also the function `assign-reward'.
See also: ACT-R parameter :alpha")
		   
(def-actup-parameter *nu* 0.0 "Utility assigned to compiled procedures.

This is the starting utility for a newly learned procedure (those created
by the production compilation mechanism). This is the U(0) value for
such a procedure if utility learning is enabled and the default utility if
learning is not enabled. The default value is 0.

See also the function `assign-reward' and the variable `*procedure-compilation*'.
See also: ACT-R parameter :nu")

(def-actup-parameter *iu* 0.0 "Initial procedure utility.

The initial utility value for a user-defined procedure (`defproc'). This is
the U(0) value for a production if utility learning is enabled and the
default utility if learning (`*ul*') is not enabled. The default value
is 0.

See also the function `assign-reward'.
See also: ACT-R parameter :iu")

(def-actup-parameter *ul* t "Utility learning flag.

If this is set to t, then the utility learning equation used above
will be used to learn the utilities as the model runs. If it is set to
nil then the explicitly set utility values for the procedures are
used (though the noise will still be added if `*egs*' is
non-zero). The default value is nil.

See also the function `assign-reward'.
Only if `assign-reward' is called will this parameter have any effect.

See also: ACT-R parameter :ul")

(def-actup-parameter *ut* nil "Utility threshold.

This is the utility threshold. If it is set to a number then that is
the minimum utility value that a procedure must have to compete in
conflict resolution. Procedures with a lower utility value than that
will not be selected. The default value is nil which means that there
is no threshold value and all procedures will be considered.

See also: ACT-R parameter :ut")

(def-actup-parameter *egs* nil "Transient noise parameter for ACT-UP procedures.

This is the expected gain s parameter. It specifies the s parameter
for the noise added to the utility values. It defaults to 0 which
means there is no noise in utilities.

See also: ACT-R parameter :egs")

(export '(*au-rpps* *alpha* *au-rfr* *iu* *nu* *ut* *ul* *egs* assign-reward))


;; tests

;; (setq *actup-procgroups* nil)
;; (defproc procedure1 (arg1 arg2) :group g1 (print arg1))
;; (defproc procedure1b (arg1 arg2) :group (g1 g5) (print arg1))
;; (defproc procedure2 (arg2 arg3) :group (g1 g2) (print arg1))
;; (defproc procedure3 (arg3 arg4) :group g2 (print arg1))
;; (equal *actup-procgroups* '((G2 (PROCEDURE3 ARG3 ARG4) (PROCEDURE2 ARG2 ARG3)) (G5 (PROCEDURE1B ARG1 ARG2)) (G1 (PROCEDURE2 ARG2 ARG3) (PROCEDURE1B ARG1 ARG2) (PROCEDURE1 ARG1 ARG2))))
;; (g1 'working 'huh)

(defun set-alist (key val alist)
  "Sets value in alist."
  (let ((kv (assoc key alist)))
    (if kv
	(rplacd kv val)
	(setf (cdr (last alist)) (list (cons key val)))))
  alist)


(defstruct proc
  name  ; also function name of procedure
  utility
  firing-time  ; :at parameter in ACT-R
)

(defstruct (compiled-proc (:include proc))
  result
  result-lambda
  result-lambda-compiled
  args
  original-proc)



(defun lookup-proc (name &optional add)
  "Look up a procedure in current model PM from procedure name.
Add a procedure object to current model PM if necessary."

  ;; look up procedure object
  (let* ((regular-procs (procedural-memory-regular-procs (model-pm (current-model))))
	 (proc (gethash name regular-procs)))
    (unless proc
      (setq proc (make-proc :name name :utility (or (get name 'initial-utility) *iu*)))
      (when add
	  (setf (gethash name regular-procs) proc)))
    proc))
   
(defmacro defproc (name args &rest body)
  "Define an ACT-UP procedure.
The syntax follows the Lisp `defun' macro, except that 
some keyword-argument parameters may follow ARGS
at the beginning of BODY.

This macro will define a Lisp function of name NAME with
arguments ARGS.  The Lisp function will execute the Lisp
forms in BODY and return the value of the last form.

The known parameters are:

 :GROUP the-group
A :group parameter defines one or or a list of procedure
groups that the procedure will belong to.  All procedures defined as part of a
group must have the same argument footprint.


If GROUP is given, a function of name GROUP will also be
defined that invokes one of the procedures assigned to GROUP.
For example:

 (defproc subtract-digit-by-addition (minuend subtrahend)
   :group subtract
   \"Perform subtraction of a single digit via addition.\"
   (let ((chunk (retrieve-chunk `(:chunk-type addition-fact
                                  :result ,minuend
                                  :add1 ,subtrahend))))
       (if chunk (addition-fact-add2 chunk))))
 (defproc subtract-digit-by-decrement (minuend subtrahend)
   :group subtract
   \"Perform subtraction of a single digit via subtraction knowledge.\"
   ...)

These procedures can be invoked via a function call such as

 (subtract 5 2)

ACT-UP will choose the procedure that has the highest utility.  See
`assign-reward' for manipulation of utilities (reinforcement
learning), and `*procedure-compilation*' for in-theory compilation of
procedures (routinization, internalization).

 :INITIAL-UTILITY u

The :initial-utility parameter sets the utility that this procedure receives when it is created or the model is reset.
If not given, the initial utility will be the value of `*iu*' at time of first invocation.

Procedure utilities, wether initial or acquired through rewards are always specific to the model.

Procedures and groupings of procedures are not specific to the model."
  ;; remove keyword args from body
  (if (member '&rest args)
      (error "defproc: &rest specifier not supported in argument list."))

  (let* ((args-filtered (filter-argument-list args))
	 (doc-string "Invoke ACT-UP procedure.")
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
			 (format-nil "defproc ~s: more than one :GROUP keyword given."
				     name)))
		    (setq group (car body)))
		   ((or (eq keyw :initial-utility) (eq keyw :iu))
		    (pop body)
		    (if iu 
			(error 
			 (format-nil "defproc ~s: more than one :INITIAL-UTILITY keyword given."
				     name)))
		    (setq iu (car body)))
		   ((keywordp keyw) t)
		   (t (return nil)))
		 (pop body))
	    (if (consp group) group (list group)))))
    (if (member name groups)
	(error (format-nil "defproc: procedure name ~a must not coincide with group name."
			   name)))
    (if (eq 'quote (car groups))
	(setq groups (second groups)))

    `(progn
       (defun-module procedural ,name ,args
	 ,doc-string
	 ;; to do: handle signals
	 (let (
	       (actup---proc (actup-proc-start ',name ,(cons 'list args-filtered)))
	       (actup---proc-result
		(progn
		  ,@body)))
	   (actup-proc-end actup---proc ',(or (unless (equal groups '(nil)) groups) (list name))
			   (list ,@args-filtered) actup---proc-result ',args-filtered ',args)
	   actup---proc-result))
       (setf (get ',name 'initial-utility) ,iu)  ; store initial utility
       (declare-proc ',groups
		     ',name ',args ',args-filtered))))

(defmacro defrule (args)
  "Alias for `defproc'.
This is provided for compatibility with some early published examples
of ACT-UP code.  Please use `defproc' instead."
  (cons 'defproc args))

(defun actup-proc-start (name args)
  (declare (ignore args))
  ;;(format-t "start: ~s ~s" name args)

  ;; look up proc object
  (let ((proc (lookup-proc name t)))
    (when *ul* ; utility learning on
      ;; add proc to queue
      (push (list (actup-time) proc)
	    (procedural-memory-proc-queue (model-pm (current-model)))))
    
    ;; return proc
    proc))

(defun make-compiled-proc-name (group args result)
  (intern (format-nil "~a/~a->~a"
		  group (length args)
		  (cond ((or (symbolp result) (stringp result)) result)
			((numberp (format-nil "~2f" result)))
			((actup-chunk-p result) (get-chunk-name result))
			(t "<S>")))))


(def-actup-parameter *procedure-compilation* nil "If non-nil, procedure compilation is enabled.
Procedure compilation causes ACT-UP procedures defined with `defproc' to be compiled (or: cached).
After execution of a source procedure,  name, execution arguments and the result are stored as
compiled procedure.  The compiled procedure is added to each of the source procedure's groups.

When the group is executed, compiled procedures compete for execution with the other procedures in the group.  (The procedure with the highest utility is chosen.)

The initial utility of a compiled procedure equals the initial utility of the source procedure.  When a source procedure is compiled multiple times, the utility of the compiled procedure is updated by assigning the source procedure utility as reward to the compiled procedure (according to the ACT-R difference learning equation).  See also `assign-reward' for reward assignment to regular procedures.

`*epl*' is defined as alias for `*procedure-compilation*'.")

(define-symbol-macro *epl* *procedure-compilation*) ; compatibility macro
(export '(*procedure-compilation* *epl*))

;; make symbols for procedural variables

;; (defvar *procedural-variables* 
;;   (loop for i from 0 to 31 collect
;;        (let ((sym (intern (format nil "VARIABLE-~a" i))))
;; 	 (setf (get sym 'actup-variable) i))))

; (setq *procedure-compilation* t)
; (actup-proc-end 'foo '(gr1 gr2) '(one two three) (list 1 'two 3) '(A B &optional C D) '(1 2 3 nil))

(forward-declare assign-reward-to-proc (util proc))
(defun actup-proc-end (this-proc groups args result args-varnames args-lambda)
  ;; possibly compile this proc

  ;; compile proc
  (when *procedure-compilation*
    ;; (format t "args-varnames: ~a ~%" args-varnames)
    ;; (format t "args: ~a ~%" args)

    ;; generalization step for args and results
    (let* ((variables-used nil)
	   (args2 (copy-list args))
	   (closure 
	    `(lambda ,args-lambda 
	       ,(cond ((and nil (listp result))  ;;;; DISABLED
		       (cons 'list
			     (loop
				for r in result
				for thisarg = (position r args :test #'eql)
				collect
				;;  (progn (format t "thisarg: (position ~a ~a :test #'eql) ==>~a~%" r args thisarg)
				  (if thisarg
				      (prog1
					(nth thisarg args-varnames)  ;; return value
					(setf (nth thisarg args2) '*) ;; set corresponding argument in args list to wildcard
					(setq variables-used t))
				      `(quote ,r)))))
		      ((and nil (or (symbolp result) (numberp result)))  ;;;; DISABLED
		       (let ((thisarg (position result args :test #'eql)))
			 (if thisarg
			     ;; set v-u to non-nil, and return value
			     (prog1
				 (nth thisarg args-varnames)  ;; return value
			       (setf (nth thisarg args2) '*) ;; set corresponding argument in args list to wildcard
			       (setq variables-used t)) 
			     `(quote ,result)) ))
		      (t 
			 `(quote ,result)))))) 
      ;;(format t "closure: ~a ~%" closure)
    (loop for group in groups 
       for leaf = (get-tree-leaf-create (procedural-memory-compiled-procs 
					 (model-pm (current-model)))
					(cons group args2))
	 do
	 (or
	  ;; procedure already present in list:
	  (if variables-used
	      ;; for lambda terms
	      (loop for proc in (cdr leaf) do
		   (when (equal closure (compiled-proc-result-lambda proc)) ;; compare uncompiled lambda term
		     ;; update utility of this proc
		     (assign-reward-to-proc (proc-utility this-proc) proc)
		     (return t)))
	      ;; for regular results
	      (loop for proc in (cdr leaf) do
		   (when (equal (list result) (compiled-proc-result proc))
		     ;; update utility of this proc
		     (assign-reward-to-proc (proc-utility this-proc) proc)
		     (return t))))
	  (let ((proc-name (make-compiled-proc-name group args2 result)))
	    (debug-print *informational* "Adding compiled procedure ~a, group ~a.~%" proc-name group)
	    (setf (cdr (last leaf))
		(list 
		 (make-compiled-proc
		  :name proc-name
		  :args args2
		  :result (if variables-used nil (list result))
		  :result-lambda (if variables-used closure)
		  :result-lambda-compiled  nil ;; compile later
		  :utility *nu* ; (proc-utility this-proc)
		  :original-proc (proc-name this-proc)
		  :firing-time (proc-firing-time this-proc)))))))))

  ;; (format-t "end: ~s ~s" name result)
  (pass-time (or (proc-firing-time this-proc) *dat*)) ;; to do: randomization (:vpft parameter)
)

(defun declare-proc (groups name args args-filtered)
  "Declares ACT-R procedure NAME,
belonging to GROUPS, taking arguments ARGS.
ARGS-FILTERED is the filtered list of arguments (as it
would occur in a function call - without &key, &rest, &optional
specifiers and defaults).
Returns NAME."
  ;; to do: check number of arguments 
  ; remove procedure from all groups first
  (loop for grs in *actup-procgroups* do
       (setf (cdr grs) (delete name (cdr grs))))
  (setq *actup-procgroups* (delete-if (lambda (x) (not (cdr x))) *actup-procgroups*))
  (setf (get name 'groups) groups)

  (loop for g in groups when g do
     ;; (re)define lisp function with group name
       ;; cannot unintern g here because it is already
       ;; exported to the user's package
       (eval `(defun-module procedural ,g ,args 
		,(format-nil "Choose a procedure out of group %s" g)
		(actup-eval-proc ',g ,@args-filtered)))
       (let ((group-cons (assoc g *actup-procgroups*)))
	 (if group-cons
	     (unless (member name (cdr group-cons))
	       (setf (cdr group-cons)  
		     (cons name (cdr group-cons))))
	     (setq *actup-procgroups*
		   (cons 
		    (list g
			  name)
		    *actup-procgroups*)))))
  name)

  

;; we cannot use a normal hash
;; because sxhash is simply not very good with objects
;; (only seems to depend on object type)
;; (defun proc-result-hash (name args)

;;   ;; problem here:
;;   ;; arguments could be chunks
;;   ;; which, internally, can be very different

;;   (sxhash (list name args)))

;; test case:
;; (defun procedure2 (a1 a2) (print a1))
;; (actup-eval-procedure 'g2 1 2)
    


(defun fire-compiled-proc (proc args)
  (debug-print *informational* "Firing compiled procedure: (~a ~{~s ~})->~a~%"
	       (compiled-proc-original-proc proc)
	       (compiled-proc-args proc)
	       (compiled-proc-result proc))
  (when *ul* ; utility learning on
    ;; add proc to queue
    (push (list (actup-time) proc)
	  (procedural-memory-proc-queue (model-pm (current-model)))))

  (pass-time *dat*)
  (aif (compiled-proc-result-lambda proc)  ;; compiled procedure?
      (apply (or (compiled-proc-result-lambda-compiled proc)
		 ;; just-in-time compilation
		 (setf (compiled-proc-result-lambda-compiled proc)
		       (eval it)))
	     args)
      (compiled-proc-result proc)))

(defun best-proc-of-group (group args)
  "Finds the best ACT-UP procedure from procedure group GROUP.
Compiled procedures are chosen using ARGS as arguments.
Returns PROCEDURE."
  (let ((regular-procs (mapcar #'lookup-proc (cdr (assoc group *actup-procgroups*))))
	(compiled-procs
	 (unless (eq args 'not-available) 
	 ;; the leaf value from the tree structure is list of COMPILED-PROC structures
	   (reduce #'append ;; maybe quite inefficient to create an extra list here?
		   (get-tree-values-with-wildcards
		    (procedural-memory-compiled-procs (model-pm (current-model)))
		    (cons group args))))))
    (unless regular-procs
      (error (format-nil "No procedures defined for group ~a." regular-procs)))                                                              
    (let* ((procs)
	  (util-proc (loop with utility = -100000.0
		    for proc in (append regular-procs compiled-procs)
		    for r-utility = (+ (or (proc-utility proc) *iu*) 
				       (if *egs* (act-r-noise *egs*) 0.0))
		    when (>= r-utility utility)
		    when (or (not *ut*) (>= r-utility *ut*))
		    do
		      (setq procs  (if (> r-utility utility) 
				       (list proc)   ; better
				       (cons proc procs)) ; as good as others
			    utility r-utility)
		    finally
		      (return (cons utility (choice procs)))))
	   (proc (cdr util-proc)))
      (debug-print *informational* 
		   "Group ~a with ~a~a matching procedures, choosing procedure ~a (Utility ~2,4f)~a~%"
		   group
		   (length regular-procs)
		   (if *procedure-compilation*
		       (format-nil "+~a" (length compiled-procs))
		       "")
		   (proc-name proc) (car util-proc) 
		   (if (cdr procs)
		       (format nil " from subset of best ~a" (length procs))
		       ""))
     (values proc (length regular-procs) (length compiled-procs)))))
      
(defun actup-eval-proc (group &rest args)
  "Evaluates an ACT-UP procedure from procedure group GROUP.
Passes arguments ARG to the lisp function representing 
the chose procedure."
  ;; chose procedure with highest utility
  ;; must randomize choice of procedure even if *egs* is nil
  (let ((proc (best-proc-of-group group args)))
      (when proc
	(if (compiled-proc-p proc) ;; compiled proc?	    
	      (fire-compiled-proc proc args) ;; (car proc) is result
	    ;; regular proc:
	    (apply (proc-name proc) args)))))


(defun assign-reward-internal (reward stop-at-0)
  "Like `assign-reward', but does not flush the procedure queue.
See also `flush-procedure-queue'."

  (let ((last-time (actup-time)))
    (debug-print *informational* "Assigning reward ~2f:~%" reward)
    (loop for rc in (procedural-memory-proc-queue (model-pm (current-model))) do
	
	 (let* ((r-time (first rc))
		(r-proc (second rc))
		(reward-portion (if (and *au-rfr* *au-rpps*)
				    (* reward
				       (+ *au-rfr*
					  (* *au-rpps* (- last-time r-time))))
				    (- reward (- (actup-time) r-time)) ; as in ACT-R
				    )))
	   ;; (debug-print *informational* "~a invoked ~2,2f sec. ago ~%" (proc-name (second rc)) (- (actup-time) r-time))
	   (if  (and *au-rfr* *au-rpps*)
		(setq reward (- reward reward-portion)
		      last-time r-time))
	   (and stop-at-0 (< reward-portion 0) (return nil))
	   (assign-reward-to-proc reward-portion r-proc)  ;; assign reward
	   ))))

(defun flush-procedure-queue ()
  "Empties the queue of procedures in the current model.
This resets the list of procedures to which rewards can be distributed (see
`assign-reward' and `assign-reward*')."

    (setf (procedural-memory-proc-queue (model-pm (current-model))) nil))


;; just a linear backpropagation over time
; quue elements: (time . hash)
(defun assign-reward (reward)
  "Assign reward to recently invoked procedures.
Distributes reward value REWARD across the recently invoked procedures.
See parameters `*au-rpps*', `*au-rfr*', `*alpha*', and `*iu*'.
See `defproc' for documentation on how to use utility when
selecting between procedures.

Reward must be greater than 0.

The reward is only distributed to procedures invoked since the last call to
`assign-reward' (or `flush-procedure-queue', or `reset-model').  See also
`assign-reward*' for a function that does not reset this set of
procedures."

  (assign-reward-internal reward nil)
  (flush-procedure-queue)
  ;; (setf (procedural-memory-proc-utilities (model-pm (current-model)))
  ;; 	  utilities)
  )

(defun assign-reward* (reward)
  "Like `assign-reward', but does not flush the procedure queue.
Only reward portions >0 are assigned to procedures, i.e., if
`*au-rfr*' or `*au-rpps*' are nil (ACT-R 6 reward propagation),
rewards are only assigned to procedures up to `reward' seconds back in time.
See also `flush-procedure-queue'."
  (assign-reward-internal reward t))

(defun assign-reward-to-proc (reward-portion proc)
  "Assign reward to a specific procedure."
  ;; assign reward
	   (let* ((current (or (proc-utility proc) *iu*))
		  (scaled (* *alpha* (- reward-portion current))))
	     (debug-print *informational* "  Assigning reward ~2,3f to ~a.~{~a~}~%" reward-portion (proc-name proc)
			  (loop with groups = (get (if (compiled-proc-p proc)
						       (compiled-proc-original-proc proc)
						       (proc-name proc))
					       'groups)
			     for g in groups
			     when g
			     collect
			       ;; for all groups that this proc belongs to
			       (multiple-value-bind (best  group-r group-c)
				      (let ((*egs* nil)
					    (*debug* nil))
				     ;; only for compiled procedures can we determine
				     ;; the best procedure overall, because only then do we know
				     ;; what the arguments were!
					(best-proc-of-group g (if (compiled-proc-p proc)
								  (compiled-proc-args proc)
								  'not-available)))
			
				 (format nil 
					 (if (eq best proc)  ; Is this the procedure that we're updating?
					     " ~a is ~a~a procedure in group ~a."
					     " ~a remains ~a~a procedure in group ~a.")
					 (proc-name best)
					 (if (> (+  group-r group-c) 1) "best" "only")
					 (if (compiled-proc-p proc) "" " regular")
					 g))))
	   (setf (proc-utility proc)
		 (+ current scaled))
	   ))



(export '(show-utilities))
(defun show-utilities ()
  "Prints a list of all utilities in the current model."

  (let ((rs)
	(compiled-procs (procedural-memory-compiled-procs (model-pm (current-model))))
	(regular-procs (procedural-memory-regular-procs (model-pm (current-model)))))
    (loop for g in act-up::*actup-procgroups* do
	 (loop for name in (cdr g) do
	      (pushnew name rs)))

    (loop for name in (sort rs (lambda (a b) (string-lessp (symbol-name a) (symbol-name b)))) do
       ;; look up proc object
	 (let ((proc (gethash name regular-procs)))
	   (format t "~a: ~2,2f~%" name
			(if proc
			    (proc-utility (lookup-proc name))
			    (if (get name 'initial-utility)
				(list (get name 'initial-utility) 'proc-iu)
				(list  *iu* '*iu*))))))
    (let ((num))
      (maptree (lambda (path v)
		 (let ((big-group (cddr (assoc (car path) act-up::*actup-procgroups*))))  ;; more than one proc in group?
		   (unless num
		     (format t "~%Compiled procs:~%")
		     (setq num t))
		   (if (cdr v)
		       (progn
			 (format t "~a:~%" path)
			 (loop for r in (sort v (lambda (a b) (> (proc-utility a) (proc-utility b)))) do  ;; WARNING: sort is destructive.  Should be OK though.
			      (format t "   ~a --> ~a: ~2,2f~%" (if big-group (format nil "[~a]" (compiled-proc-original-proc r)) "") 
				      (or (compiled-proc-result-lambda r) 
					  (car (compiled-proc-result r)))
					  (proc-utility r))))
		       (format t "~a ~a --> ~a: ~2,2f~%" path (if big-group (format nil "[~a]" (compiled-proc-original-proc (car v))) "") 
			       (or (compiled-proc-result-lambda (car v)) 
				   (car (compiled-proc-result (car v))))
			       (proc-utility (car v))))))
	       compiled-procs)))
  nil)

;; experimental

;; maybe we'll leave all parameters global for now
;; (defun switch-to-model (model)

;;   ;; handle dynamic, global variables as model-parameters
;;   (loop for p in *actUP-model-parameters* do
;;        (let ((sym (format-nil "*~a*" p)))


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

(actup-load "au-visual" "modules")

(actup-load "au-manual" "modules")

(defparameter *act-up-version* 0.1 "Version of a loaded ACT-UP.
ACT-UP has been correctly initialized if this is defined and non-nil.")
(export '(*act-up-version*))

(provide "act-up")