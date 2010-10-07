;; act-r 6 compatibility code
;; it's a big hack.


(defun chunk-spec-variable-p (chunk)
  chunk)

;; (load (format-nil "~a/misc-utils.lisp" (directory-namestring *load-truename*)))

(defmacro define-module-fct (&rest args)
  (declare (ignore args))
  nil)

(defvar *actr-random-module* nil)

(defmacro get-module (m)
  (if (eq m 'random-module)
      '*actr-random-module*
      nil))



;;; print-warning
;;;
;;; (defmacro print-warning (control-string &rest args))
;;;
;;; control-string is a control-string as would be passed to the format function
;;; args are the arguments to use in that control string
;;;
;;; control-string and args are passed to format on the stream *error-output* 
;;; with the text "#|Warning: " proceeding it and "|#" after it so that it would 
;;; appear as a comment if the stream were to be read.
;;;
;;; nil is returned.  

(defmacro print-warning (message &rest arguments)
  "Outputs a warning of message and arguments."
  `(format *error-output* "~&#|Warning: ~@? |#~%" ,message ,@arguments))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; expt-coerced, exp-coerced, log-coerced, and sqrt-coerced
;;;
;;; These are for improved speed in Lisps that use doubles for math so that
;;; it can coerce them to single when that's the setting of 
;;; *read-default-float-format*.  Really makes a difference in MCL...
;;; Actually, in MCL 5 it makes no difference, and may want to just 
;;; eliminate this...

(defmacro expt-coerced (base power)
  "Computes expt and coerce to *read-default-float-format* if needed"
  (if (typep (expt 1.0 1.0) *read-default-float-format*)
    `(expt ,base ,power)
    `(coerce (expt ,base ,power) ,*read-default-float-format*)))

(defmacro exp-coerced (arg)
  "Computes expt and coerce to *read-default-float-format* if needed"
  (if (typep (expt 1.0 1.0) *read-default-float-format*)
    `(exp ,arg)
    `(coerce (exp ,arg) ,*read-default-float-format*)))

(defmacro log-coerced (arg &optional (base nil basep))
  "Computes log and coerce to *read-default-float-format* if needed
   doesn't accept a base however"
  (if (typep (log 1.0) *read-default-float-format*)
      (if basep
          `(log ,arg ,base)
        `(log ,arg))
    (if basep
        `(coerce (log ,arg ,base) ,*read-default-float-format*)
      `(coerce (log ,arg) ,*read-default-float-format*))))

(defmacro sqrt-coerced (arg)
  "Computes sqrt and coerce to *read-default-float-format* if needed"
  (if (typep (sqrt 2.0) *read-default-float-format*)
    `(sqrt ,arg)
    `(coerce (sqrt ,arg) ,*read-default-float-format*)))


;; allow random module to work
(load (format-nil "~a/random.lisp" (directory-namestring *load-truename*)))


(setq  *actr-random-module* (create-random-module nil))

