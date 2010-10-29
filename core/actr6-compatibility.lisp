;; act-r 6 compatibility code
;; it's a big hack.


(defun chunk-spec-variable-p (chunk)
  chunk)

(actup-load "misc-utils")

(defmacro define-module-fct (&rest args)
  (declare (ignore args))
  nil)

(defvar *actr-random-module* nil)

(defmacro get-module (m)
  (if (eq m 'random-module)
      '*actr-random-module*
      nil))

;; allow random module to work
(actup-load "random")


(setq  *actr-random-module* (create-random-module nil))

