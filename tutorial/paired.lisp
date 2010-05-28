;; paired associates model in ACT-UP

(require "act-up" "../act-up.lisp")
(use-package :act-up)

(require "act-up-experiments" "../act-up-experiments.lisp")
(use-package :act-up-experiments)


(defvar *pairs* '(("bank" "0") ("card" "1") ("dart" "2") ("face" "3") ("game" "4")
                  ("hand" "5") ("jack" "6") ("king" "7") ("lamb" "8") ("mask" "9")
                  ("neck" "0") ("pipe" "1") ("quip" "2") ("rope" "3") ("sock" "4")
                  ("tent" "5") ("vent" "6") ("wall" "7") ("xray" "8") ("zinc" "9")))

 
(define-slots probe answer)
 
; (macroexpand '(define-slots 'probe 'answer))

; (learn-chunk (make-chunk first 1 second 2))


(setq *rt* -2
      *lf* 0.4
      *ans* 0.5
      *bll* 0.5)

(defun learn-associate (probe answer trial)
  ; first, we see the probe only.
  ; we attempt to retrieve something for the probe.

  ;; probe is shown now

  (let ((t0 (actup-time))
	(ch (retrieve-chunk `(:probe ,probe) nil nil 5.0)))

    (if ch  ; chunk found
	(aggregate (list nil (- (actup-time) t0)) (list trial))
	(progn
	  ;; we wait until 5 seconds are over
	  
	  (print (- (actup-time) t0))
	  (actUP-pass-time (- 5 (- (actup-time) t0)))))


    ;; now, we get to see the digit and we can learn the chunk

    (learn-chunk (make-chunk :probe probe :answer answer))

    ;; we see it for 5 seconds

    (actUP-pass-time 5)

    ;; if retrieved, we type the probe's number (not modeled)
    (if ch (chunk-answer ch) nil)))

(defun run-exp ()

  (clear-aggregates '("trial accuracy"))

  (loop for subj from 1 to 20 do
       (reset-model) ; different subject
       (loop for trial from 1 to 8 do
	    (loop for (probe answer) in *pairs* do
		 (aggregate (list
			     (if (equal answer (learn-associate probe answer trial))
				 1 0) nil)
			    (list trial)))))
  
  (print-aggregates))