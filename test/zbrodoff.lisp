;;; Filename: zbrodoff.lisp

;; Zbrodoff Unit test


;; These load commands will find the ACT-UP files
;; relative to the location of the present file:
(load (concatenate 'string (directory-namestring *load-truename*) "../tutorial/zbrodoff.lisp"))

;; Architectural parameters - not set in tutorial version
(setq *ans* 0.5)
(setq *rt* 1.2)

;; Model parameters:
(setq *model-time-parameter-1* 0.55)
(setq *model-time-parameter-2* 0.54)

;;(defparameter *model-time-parameter-1* 0.5)
;;(defparameter *model-time-parameter-2* 0.6)


;; Alternative, using *lf* from the ACT-R Tutorial:
;; (setq *lf* 0.4)
;; (defparameter *model-time-parameter-1* 0.6)
;; (defparameter *model-time-parameter-2* 0.8)




;; With the ACT-R default parameters (*lf*=1.0)
;; Best correlation: 0.9972 at parameters (0.4 0.41)
;; Best mean deviation: 0.0549 at parameters (0.5499999 0.5399999)
;; manual selection (via plots, and in R)
;; suggests 0.55 and 0.54, giving these:
;; CORRELATION:  0.994
;; MEAN DEVIATION:  0.066

;; With the LF parameter from the ACT-R tutorial (*lf*=0.4)
;; Best correlation: 0.9971 at parameters (0.4 0.45000002)
;; Best mean deviation: 0.0775 at parameters (0.6 0.8000001)
;; CORRELATION:  0.992
;; MEAN DEVIATION:  0.095


;; R code to plot and inspect the results:
;; d <- read.table("zbrodoff-results.txt", header=T)
;; library (Hmisc)  # may need to be installed!
;; xYplot(deviation~time1+time2, data=d, method=smean.cl.boot)
;; xYplot(deviation~I(time1+time2), data=d, method=smean.cl.boot)
;; d2 = subset(d, time1+time2>1.07 & time1+time2<1.12)
;; subset(d2, correlation==max(d2$correlation))