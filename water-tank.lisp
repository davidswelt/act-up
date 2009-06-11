;; DSF Model
;; David Reitter / CMU
;; 05/13/2009

;; Problems?  Please e-mail reitter@cmu.edu


;; USAGE

;; (load "water-tank.lisp")

;; (run-socket &optional host port)
;; e.g.
;; (run-socket "localhost")

;; if it can't connect, try again 



;; Parameters  (set below, lines 166-)
;; (setq *blend-temperature* 0.5)
;; (defparameter *goal-level* 4.0)
;; (defparameter *calc-time-factor* 1.0)
;; (defparameter *environ-wait* 8)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; benchmark:
;; about 45 iterations / sec
;; on MBP with openmcl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs stuff
(when nil
;; emacs side
  (progn 
    (server-start)
    (slime)
    (find-file "water.R")
    (R)
    (process-send-string (get-ess-process ess-current-process-name) "source('/users/dr/MURI/ACT-UP/water.R')\n"))
)




;; Libraries

(declaim (optimize (speed 1) (space 0) (debug 3)))

(require "act-up" "act-up.lisp")
(use-package :act-up)
(load "split-sequence.lisp")

;; MODEL

(defparameter *blend-temperature* 0.25)

(defstruct (trend (:include chunk)) ;; (concept (:print-object print-concept))
  (type  'trend)
  trend
) 
(defstruct (strategy (:include chunk))
  (type  'strategy)
  strategy
  dampen
  success
) 



(defparameter *predictedWaterLevel* nil)
(defparameter *guesstimate* nil)
(defparameter *strategies* nil)

(defun calc-1 (goal current trend0 trend1)
  `(+ ,goal (+ ,current ,trend0 ,trend1)))
(defun calc-2  (goal current trend0 trend1)
  `(- ,goal (- ,current ,trend0 ,trend1)))
(defun calc-3  (goal current trend0 trend1)
  `(+ ,goal (- ,current ,trend0 ,trend1)))
(defun calc (goal current trend0 trend1)
  `(- ,goal (+ ,current ,trend0 ,trend1)))
(defun model-init ()
  (reset-mp)
  (reset-model)
  (setq  *guesstimate* t *l-trend0* nil *l-trend1* nil *last-strategy* nil)
  (dotimes (n 1)

    ;; a set of possible guessing and calculation strategies
    (setq *strategies* '(guess calc calc-1 calc-2 calc-3))
    (learn-chunk  (make-strategy :strategy 'guess :success 0.2))
    (learn-chunk  (make-strategy :strategy 'calc :success 0.2
				 ))
    (actup-pass-time 0.3)
    
    (learn-chunk  (make-strategy :strategy 'calc-1 :success 0.2
   				))
    (learn-chunk  (make-strategy :strategy 'calc-2 :success 0.2 
   				))
    (learn-chunk  (make-strategy :strategy 'calc-3 :success 0.2 
   				)))
  ;; source of subject-specific variance
  (learn-chunk  (make-trend :trend (- 10 (random 20)) :type 'l-trend0))
  (learn-chunk  (make-trend :trend (- 10 (random 20)) :type 'l-trend0))
  ;; (learn-chunk  (make-trend :trend 0.0 :type 'trend1))
  (learn-chunk  (make-trend :trend -8.0 :type 'trend2))
  ;; maybe the initial (bordering) chunks need
  ;; much more activation so that they don't decay
)

 

(defun retrieve-chunk (spec &optional cues)
  (best-chunk (filter-chunks (model-chunks *current-actUP-model*)
			     spec)
	      cues spec))

(defun retrieve-pm-chunk (hard-spec soft-spec &optional cues)
  (best-chunk (filter-chunks (model-chunks *current-actUP-model*)
			     hard-spec)
	      cues (append hard-spec soft-spec)))
(defun blend-retrieve-chunk (spec &optional cues)
  (let ((cs (filter-chunks (model-chunks *current-actUP-model*)
			     spec)))
    (if cs
	(blend cs cues nil spec))))
    
(defun trend-trend-safe (trend-chunk)
  (if trend-chunk
      (trend-trend trend-chunk)
      nil))


(defun safe-strategy-strategy (strategy-chunk)
  (if strategy-chunk
      (strategy-strategy strategy-chunk)
      'calc))

(defun best-by-blend-activation (chunks)
  (loop for c in chunks with ba=0 with bc=nil
     finally (return bc)
     when c 
     when (> (chunk-last-activation c) ba)
     do
       (setq bc c ba (chunk-last-activation c))))




(defmacro tround (value)
 `(* (round ,value 0.1) 0.1))

(defvar *l-trend0* nil)

(defvar *l-trend1* nil)

(defvar *last-strategy* nil)



;;  PARAMETERS

(setq *blend-temperature* 0.5)
(defparameter *goal-level* 4.0)
(defparameter *calc-time-factor* 1.0)
(defparameter *environ-wait* 8)

(defun model-get-inflow (waterLevel envirInflow)
  (declaim (optimize (speed 1) (space 0) (debug 3)))

  ;;  (format t "wl:~a i:~a ~%" waterLevel envirInflow)

  (let* (
	 (current-trend 0) (current-d1 0) (current-d2 0)
	 (last-observed-trend   (if *guesstimate* (trend-trend-safe (retrieve-chunk '(:type l-trend0))) *l-trend0*))
	 (last-observed-d1 (if *guesstimate* (trend-trend-safe (retrieve-chunk '(:type l-trend1))) *l-trend1*)))


  ;; learn previous action

    ;; observe predicted vs. actual water level
    ;; predicted does not contain any trend  (maybe change?)

    (when *predictedWaterLevel*
      (setq current-trend envirInflow) ;; (- waterLevel *predictedWaterLevel*   ))
      (learn-chunk (make-trend :trend (tround current-trend))))

    ;; learn first derivative
    (when last-observed-trend
      
      (setq current-d1  (- current-trend last-observed-trend ))
      (learn-chunk (make-trend :type 'trend1 :trend (tround current-d1))))

    ;; learn second derivative
    (when last-observed-d1
      (setq current-d2 (- current-d1 last-observed-d1 ))
      (learn-chunk (make-trend :type 'trend2 :trend (tround current-d2))))

   

;; how can we learn the first derivative?

;; 0.: change in water level (beyond expectation from our own correction)
;; 1.: change in 0 compared to last measurement

        ;; learn strategy

    (when *predictedWaterLevel* 
      (if *guesstimate*
	  (learn-chunk (make-strategy :strategy 'guess
				      :success (tround (abs (- *predictedWaterLevel* waterLevel)))))
	  
	  (when  *last-strategy*
	    (learn-chunk (make-strategy :strategy (strategy-strategy *last-strategy*)
					:success (tround (abs (- *predictedWaterLevel* waterLevel))))))))
    

    (actUP-pass-time 2.5)
    
    ;; (retrieve-pm-chunk '(:type strategy) '(:success 0) 
    ;;					       nil)
    (let ((current-strategy (best-chunk  ;; retrieve blended 
			     (loop for s in *strategies* collect  
				  (blend (filter-chunks (model-chunks *current-actUP-model*)
							`(:type strategy :strategy ,s)) 
					 nil ;; no cues
					 'strategy ;; return type
					 `(:strategy ,s) ;; retrieval spec
					 ))
			     nil ;; no cues
			     `(:success 0.0) ;; retrieval spec
			     ))
	  (dampen nil))


      (unless current-strategy
	(setq current-strategy (make-strategy :strategy 'guess)))

      (setq *guesstimate* (eq 'guess (safe-strategy-strategy current-strategy)))
      (setq dampen (strategy-dampen current-strategy))
      
      ;; it's currently unclear how we would
      ;; decide whether damping is needed.
	  

  ;; retrieve chunk  
  (let* ((assumed-trend  envirInflow)

	 (expected-trend-1-chunks  (filter-chunks (model-chunks *current-actUP-model*)
							       '(:type trend1)))
	 (expected-trend-1-best (if *guesstimate*
				    (best-chunk expected-trend-1-chunks nil)
				    (make-trend :trend current-d1)
				    ))
	 (expected-trend-1-est (blend expected-trend-1-chunks nil 'trend))
	 (assumed-trend-1 (trend-trend 
			   (or 
			    (if (or *guesstimate* )
				(or expected-trend-1-est expected-trend-1-best)
				expected-trend-1-best) (make-trend :trend 0))))
	 (needed-effect (if (or *guesstimate*  (not current-strategy))
			    (- *goal-level* (+ waterLevel assumed-trend  assumed-trend-1 ))
			    (cogn-calculate (funcall (strategy-strategy current-strategy) *goal-level* waterlevel assumed-trend assumed-trend-1))))
	 (chosen-valve-setting needed-effect))

   ;; DAMPING
    (when dampen ;; damping
      (let ((surprisal (- waterLevel *predictedWaterLevel*)))
    	(if (< surprisal -1)
    	    (setq needed-effect-1 (- needed-effect-1 (* 0.3 surprisal))))
    	(if (> surprisal 1)
    	    (setq needed-effect-1 (- needed-effect-1 (* 0.3 surprisal)))))
      (setq chosen-valve-setting (- *goal-level* (+ waterLevel needed-effect-1))))

    (format t "strat: ~a level: ~a   needed: ~a    assumed-trend: ~,2F assumed-trend':  ~,2F  assumed-trend'':  ~,2F chosen valve:~a  ~%" 
	    (or (strategy-strategy current-strategy) (strategy-strategy current-strategy))
	    waterLevel needed-effect  assumed-trend  assumed-trend-1 'na chosen-valve-setting)
     
    (setq *predictedWaterLevel* *goal-level*) ;; (+ waterLevel chosen-valve-setting assumed-trend-1 assumed-trend-2))

    (actUP-pass-time (* *calc-time-factor* (if *guesstimate* 5 10))) ;; seconds

    (learn-chunk (make-trend :type 'l-trend0 :trend (tround current-trend)))
    (learn-chunk (make-trend :type 'l-trend1 :trend (tround current-d1)))


    (when (not *guesstimate*)
      (setq *l-trend0* (tround current-trend))
      (setq *l-trend1* (tround current-d1)))
    (setq *last-strategy* current-strategy)
    

    (actUP-pass-time *environ-wait*) ;; seconds  ;; this time seems to be crucial - longer, more noise in memembering
    chosen-valve-setting
    ))))

(defun cogn-calculate (expression)
  ;; error probabilities are guesses
  ;; individual differences should be realized here

  ;; to do: pass-time

  (let* ((err-prob 0.0)
	 (res
	  (cond
	    ((numberp expression) expression)
	    ((eq (car expression) '+)
	     ;; addition
	     (setq err-prob 0.03)
	     (apply #'+ (mapcar 'cogn-calculate (cdr expression))))
	    ((eq (car expression) '-)
	     ;; subtraction
	     (setq err-prob 0.06)
	     (apply #'- (mapcar 'cogn-calculate (cdr expression))))
	    ((eq (car expression) '*)
	     ;; multiplication
	     (setq err-prob 0.13)
	     (apply #'* (mapcar 'cogn-calculate (cdr expression))))
	    ((eq (car expression) '/)
	     ;; division
	     (setq err-prob 0.20)
	     (apply #'/ (mapcar 'cogn-calculate (cdr expression)))))))
    (if (< (/ (random 100 my-random-state) 100) err-prob)
	(* res (+ 0.5 (/ (random 10 my-random-state) 10.0)))
	(+ 0.0 res))))


; (cogn-calculate '(+ 4 (/ 5 2)))



;;; SIMULATION ENVIRONMENT

;; environment
(defparameter *stats-output* nil)
(defvar *step* nil)
(defun run (&optional num-subjects)
  
  (let ((stats-output (open "/Users/dr/MURI/ACT-UP/water.txt" 
			    :direction :output :if-exists :supersede :if-does-not-exist :create)))
    (setq *stats-output* stats-output)
    (format *stats-output* "envWait calcTimeFac blendTemp subject step Version trend waterLevel chosenValveSetting~%")

;; (loop for *blend-temperature* in '(0.5 1.0) do
;; (loop for *environ-wait* from 3 to 10 do
;; (loop for *calc-time-factor* from 0.4 to 2 by 0.15 do

    (loop for subject from 1 to (or num-subjects 1) do

	 (loop for (fun-name . fun) in (list 
					;; (cons "constant" (lambda (step) 2.5))
					(cons "Linear increase" (lambda (step) (+ 2 (* step 0.08))))
					(cons "Linear decrease" (lambda (step) (+ 10 (* step -0.08))))
					(cons "Non Linear increase" (lambda (step) (* 5 (log step 10))))
					(cons "Non Linear decrease" (lambda (step) (* 5 (log (- 101 step) 10))))
					;; (cons "Sine" (lambda (step) (+ 5 (* (sin (* pi 2 (/ step 100))) 4))))
					;; (cons "Sine2" (lambda (step) (+ 5 (* (sin (* pi 5 (/ step 100))) 4))))
					)
	    do
	      (format t "~a~%" fun-name)
	      (model-init)
	      (let ((modelInflow-queue '( )) ;; configure delay here
		    (waterLevel 2.0))

		(setq modelInflow-queue (append modelInflow-queue 
						(list (model-get-inflow waterLevel 
									0 ;; inflow not shown to the user
									))))

		(loop for step from 1 to 100 do
		     (setq *step* step)
		     (let ((chosenModelInFlow (car (last modelInflow-queue)))
			   (envirInflow (funcall fun step)))
		       
		       (setq waterLevel (+ waterLevel envirInflow))
		       (setq waterLevel  (+ waterLevel (car modelInflow-queue)))
		       (setq waterLevel (max 0 waterLevel))
		       (setq modelInflow-queue (cdr modelInflow-queue))

		       (format *stats-output* "~a ~a ~a ~a ~a \"~a\" ~,5F ~,5F ~,5F ~%"
			       *environ-wait* *calc-time-factor* *blend-temperature* subject step fun-name envirInflow waterLevel chosenModelInFlow)

		       (setq modelInflow-queue (append modelInflow-queue 
						       (list (model-get-inflow waterLevel envirInflow))))
		       )) ;)))
     )))
(close stats-output))
(update-emacs-R))

(defun run-trace (&optional num-subjects)
  
  (let ((data-input (open "/Users/dr/MURI/ACT-UP/Challenge/human_data.csv"
			    :direction :input)) 
	(stats-output (open "/Users/dr/MURI/ACT-UP/water-trace.txt" 
			    :direction :output :if-exists :supersede :if-does-not-exist :create))
	(current-subject))
    (setq *stats-output* stats-output)
    (format *stats-output* "subject step Version trend waterLevel chosenValveSetting humanValveSetting~%")
     
    (read-line data-input nil 'eof)
    (loop for data-line = (read-line data-input nil 'eof)
	    until (eq data-line 'eof)
	    do
	      (let ((data (split-sequence:SPLIT-SEQUENCE #\, data-line )))
		(print data-line)
		;; Version,Subject,Time Step,EnvirInFlow,EnvirOutFlow,UserInFlow,UserOutFlow,AmountInTank,Goal

		(let ((fun-name (nth 0 data))
		      (subject (nth 1 data))
		      (step  (read-from-string (nth 2 data)))
		      (envirInflow  (read-from-string (nth 3 data)))
		      (waterLevel  (read-from-string (nth 7 data)))
		      (humanFlow (- (read-from-string (nth 5 data))  (read-from-string (nth 6 data)))))

		  (when (not (equal subject current-subject))
		    (setq current-subject subject)
		    (model-init))
		  (let ((chosenModelInFlow  (model-get-inflow waterLevel envirInflow)))
		    (format *stats-output* "~a ~a \"~a\" ~,5F ~,5F ~,5F ~,5F ~%" subject step fun-name envirInflow waterLevel chosenModelInFlow humanFlow)))))
    (close data-input)
    (close stats-output))
  (update-emacs-R))


;; Debugging

(defun pc ()
  "Print trend chunks"
  (loop for c in (model-chunks *current-actup-model*) when (trend-p c) do
       (format t "~a trend=~,2F  (~,2F, ~a)~%" (trend-type c) (trend-trend c) (chunk-get-activation c) (chunk-first-presentation c))))


(defun update-emacs-R ()
  (ccl::run-program "~/sv.aquamacs-emacs.git/nextstep/Aquamacs.app/Contents/MacOS/bin/emacsclient"
		    (list "-eval" "(process-send-string (get-ess-process ess-current-process-name) \"pl()\\n\")")
		    :output t ))
 
       
;; 
(defun test-blending ()
  (model-init)
  (format t "c(")
  (loop for i from 1 to 500 do
       (learn-chunk  (make-trend :trend (+ 10.0 (act-r-noise 0.3))))
       (actUP-pass-time 15)
       (format t "~a, " (trend-trend (blend (filter-chunks (model-chunks *current-actUP-model*)
							   '(:type trend)
							   ) nil 'trend))))
  (format t " NA)~%~%"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SOCKET API

(require 'uni-files "uni-files.lisp")
(defstruct water-state
DSFState
time
goal
amount
ui
uo
ei
eo)

(defvar *last-conn-data* nil)
(defvar *last-water-state* nil)
(defun conn-read ()  
  (let ((data (read-from-string (uni-socket-read-line *v*))))
    (when (listp data)
      (print data)
      ;; (STATE TIME 2 GOAL 4.0 AMOUNT 4.0 EI -1.0 EO -1.0 UI 0.0 UO 0.0) 
      (setq *last-conn-data* data)
      (setq *last-water-state*
	  (make-water-state
	   :DSFState (nth 0 data)
	   :time     (nth 2 data)
	   :goal     (nth 4 data)
	   :amount   (nth 6 data)  
	   :ui       (nth 12 data)
	   :uo       (nth 14 data)
	   :ei       (nth 8 data)
	   :eo       (nth 10 data))))))

(defparameter *dsf-host* nil)
(defparameter *dsf-port* nil)

(defparameter *v* nil)
(defun connsoc (entry-id name version)
  (when *v*
    (close *v*)
    (setq *v* nil))
  (setq *v* (uni-make-socket *dsf-host* *dsf-port*))
  (setq version-type "NONBATCH")
  (setq *last-water-state* (make-water-state))
  (conn-reset)
  (uni-send-string
   *v*
   (format nil "~a ~a ~a ~a" entry-id name version version-type)))
; DR DR1 1.0


(defun conn-decide (ui uo)
  ;; why ei / eo ????
  (uni-send-string *v* (format nil "DECISION UI ~d UO ~d EI ~d EO ~d" ui uo
			       (water-state-ei *last-water-state*) (water-state-eo *last-water-state*)))
;  (pause 0.3)
  (conn-read))

(defun conn-reset ()
 (uni-send-string *v* "SIMULATION RESET")
 (conn-read))
(defun conn-quit ()
 (uni-send-string *v* "QUIT"))

(defun run-socket (&optional host port)

  (setq *dsf-host* (or host "localhost") ;; "192.168.1.22")
	*dsf-port* (or port 9548))
 
  (connsoc "ENTRY-ID" "reitter-01" "VERSION")

  (conn-read)

  (while (and (not (uni-stream-closed *v*))
	      (not (equal (water-state-DSFState *last-water-state*) 'END_PROGRAM)))
		   
    (setq *goal-level* (water-state-goal *last-water-state*))
    
    (let ((new-model-inflow
	   (model-get-inflow 
	    (or (water-state-amount *last-water-state*) 0) ;; current water level
	    (- (max 0 (or (water-state-ei *last-water-state*) 0)) 
	       (max 0 (or (water-state-eo *last-water-state*) 0))) ;; inflow
	    )))
      (print new-model-inflow)
      (if (> new-model-inflow 0)
	  (conn-decide new-model-inflow 0)
	  (conn-decide 0 (- new-model-inflow)))
      (print *last-water-state*)
      )))

;; (conn-read)