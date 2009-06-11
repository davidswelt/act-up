;; act-r 6 compatibility code
;; it's a big hack.


(defun chunk-spec-variable-p (chunk)
  chunk)

(load "misc-utils.lisp")

(defmacro define-module-fct (&rest args)
	  nil)

(defvar *actr-random-module* nil)

(defmacro get-module (m)
  (if (eq m 'random-module)
      '*actr-random-module*
      nil))

;; allow random module to work
(load "random.lisp")

(setq  *actr-random-module* (create-random-module nil))

