;; -*-Mode: Act-up; fill-column: 75; comment-column: 50; -*-
;;; Filename: choice.lisp

;; Choice unit test

;; These load commands will find the ACT-UP files
;; relative to the location of the present file:
(load (concatenate 'string (directory-namestring *load-truename*) "../tutorial/choice.lisp"))

;; Architectural (ACT-R) Parameters
(setq *egs* 0.7)

;; Model parameter
(setq *positive-reward* 2.0)

;; -*-Mode: ACT-UP; fill-column: 75; comment-column: 50; -*-
