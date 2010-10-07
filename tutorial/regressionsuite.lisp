
(setq *correlation-choice* nil)
(setq *meandev-choice* nil)
(setq *correlation-fan* nil)
(setq *meandev-fan* nil)
(setq *correlation-paired* nil)
(setq *meandev-paired* nil)
(setq *correlation-siegler* nil)
(setq *meandev-siegler* nil)
(setq *correlation-pm* nil)
(setq *meandev-pm* nil)

(load "choice.lisp")

(defun cdatachoice (n)
   (let (data)
     (dotimes (i n)
       (reset-model)
       (push (do-n-blocks-of-m-trials 4 12) data))
     (set-results-choice (analyze data))))

(defun set-results-choice (results)
  (setq *correlation-choice* (correlation results *choice-data*)) 
  (setq *meandev-choice* (mean-deviation results *choice-data*)))

(cdatachoice 100)

(load "fan.lisp")

(defun aplocation ()
  (set-results-fan 
   (mapcar #'(lambda (x y) 
	       (list (/ (+ (car x) (car y)) 2.0) 
		     (and (cadr x) (cadr y))))
	   (do-person-location 'person) 
	   (do-person-location 'location))))

(defun set-results-fan (data)
  (let ((rts (mapcar 'first data)))
    (setq *correlation-fan* (correlation rts *person-location-data*))
    (setq *meandev-fan* (mean-deviation rts *person-location-data*))))

(aplocation)

(load "paired.lisp")
   
(defun cdatapaired (n)
  (do ((count 1 (1+ count))
       (results (do-experiment 20 8)
                (mapcar #'(lambda (lis1 lis2)
                            (list (+ (first lis1) (first lis2))
                                  (+ (or (second lis1) 0) (or (second lis2) 0))))
                  results (do-experiment 20 8))))
      ((equal count n) 
       (odata results n))))

(defun odata (data n)
   (let ((probability (mapcar #'(lambda (x) (/ (first x) n)) data))
        (latency (mapcar #'(lambda (x) (/ (or (second x) 0) n)) data)))
    (presults latency *paired-latencies*)
     (presults probability *paired-probability*)))
    
(defun presults (predicted data)
  (setq *correlation-paired* (correlation predicted data))
  (setq *meandev-paired* (mean-deviation predicted data)))

(cdatapaired 1000)

(load "siegler.lisp")

(defun rsubjects (n)
  (let ((responses nil))
    (dotimes (i n)
      (push (do-one-set) responses))
    (setq *responses* responses)
    (aze responses)))

(defun aresponses (responses)
  (mapcar (lambda (x) 
             (mapcar (lambda (y) 
                       (/ y (length responses))) x))
     (apply #'mapcar 
            (lambda (&rest z) 
              (let ((res nil))
                (dolist (i '(zero one two three four five six seven eight))
                  (push (count i z ) res)
                  (setf z (remove i z )))
                (push (length z) res)
                (reverse res)))
            responses)))

(defun aze (responses)
  (dresults 
   (aresponses responses)))

(defun dresults (results)
    (setq *correlation-siegler* (correlation results *siegler-data*))
    (setq *meandev-sielger* (mean-deviation results *siegler-data*)))

(rsubjects 1000)

(load "tuto5pm.lisp")

(defun apl ()
  (opl 
   (mapcar #'(lambda (x y) 
	       (list (/ (+ (car x) (car y)) 2.0) 
		     (and (cadr x) (cadr y))))
	   (do-person-location 'person) 
	   (do-person-location 'location))))

(defun opl (data)
  (let ((rts (mapcar 'first data)))
    (setq *correlation-pm* (correlation rts *person-location-data*))
    (setq *correlation-pm* (mean-deviation rts *person-location-data*))))

(apl)

(if (> *correlation-choice* 0.90)
    (if (> *meandev-choice* 0.1)
	(print '(Choice model FAIL))
      (print '(Choice model OK)))
  (print '(Choice model FAIL)))

(if (> *correlation-fan* 0.90)
    (if (> *meandev-fan* 0.1)
	(print '(Fan model FAIL))
      (print '(Fan model OK)))
  (print '(Fan model FAIL)))

(if (> *correlation-paired* 0.90)
    (if (> *meandev-paired* 0.1)
	(print '(Paired model FAIL))
      (print '(Paired model OK)))
  (print '(Paired model FAIL)))

(if (> *correlation-siegler* 0.90)
    (if (> *meandev-siegler* 0.1)
	(print '(Siegler model FAIL))
      (print '(Siegler model OK)))
  (print '(Siegler model FAIL)))

(if (> *correlation-pm* 0.90)
    (if (> *meandev-pm* 0.1)
	(print '(Partial Matching model FAIL))
      (print '(Partial Matching model OK)))
  (print '(Partial Matching model FAIL)))



