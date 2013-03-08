;; Temporal module

;; existing models need to be reset (reset-model) to add this module after loading.

;; Implementation follows work by Niels Taatgen, Hedderik van Rijn & John R. Anderson (2005)

(in-package :act-up)

;; (defmacro def-actup-parameter (name init-value &optional doc-string module type)


;;;;;  MODULE PARAMETERS

(def-actup-parameter *time-master-start-increment* 1.1d0 "Start increment parameter for time module." 'time) 
(def-actup-parameter *time-noise* 0.015d0 "Noise parameter for time module." 'time)
(def-actup-parameter *time-mult* 1.1d0 "Start increment parameter for time module." 'time)



;;;;;  MODULE DATA STRUCTURES AND MAINTENANCE

(defstruct time-memory
  (tick-reset-time (actup-time))
  (start-increment 1.1))

(defun time-init ()
  ;; return persistent storage object
  (let ((m (make-time-memory)))
    ;; not documented, but taken from the time module in ACT-R 6
    (setf (time-memory-start-increment m)
	  (+ *time-master-start-increment* 
	     (act-r-noise (* *time-master-start-increment* 0.075))))
    m))

(define-module 'time #'time-init)

;;;;;  EXPORTED MODULE FUNCTIONS

(defun-module time time-reset ()
  "Reset the time timer to 0 ticks."
  (time-init)
  (setf (time-memory-tick-reset-time act-up-module-memory) (actup-time)))

(defun-module time time-ticks ()
  "Get the current number of ticks from the time module.
The implemented algorithm is iterative as of now."

    ;; iterative algorithm
    ;; this does not scale very well.
    ;; to do: use memoization or find a closed-form function
    (let ((tick-len (+ *time-master-start-increment* 
		       (act-r-noise (* *time-noise*  ;; times 5? (PDF document suggests as much)
				       (time-memory-start-increment act-up-module-memory)))))
	  (time (time-memory-tick-reset-time act-up-module-memory))
	  (ticks -1))
      (while (< time (actup-time))
	(incf ticks)
	(incf time tick-len)



	(setq tick-len (+ (* *time-mult* tick-len) 
			  (act-r-noise (* *time-noise* 
					  *time-mult* 
        				  tick-len)))))
      ticks))
