; addition in ACT-UP

(require "act-up" "../act-up.lisp")
(use-package :act-up)

(define-slots first second arg1 arg2 sum count)

(reset-model)
(learn-chunk (make-chunk first 0 second 1))
(learn-chunk (make-chunk first 1 second 2))
(learn-chunk (make-chunk first 2 second 3))
(learn-chunk (make-chunk first 3 second 4))
(learn-chunk (make-chunk first 4 second 5))
(learn-chunk (make-chunk first 5 second 6))
(learn-chunk (make-chunk first 6 second 7))
(learn-chunk (make-chunk first 7 second 8))
(learn-chunk (make-chunk first 8 second 9))
(learn-chunk (make-chunk first 9 second 10))


(defun initialize-addition (num1 num2)

  (add num1 num2 0))

(defun add (num1 num2 count)

  (unless (= num2 count)
    (let ((r-chunk (retrieve-chunk `(:first ,num1))))
    
      ;; not finishing this for lack of empirical data.
