;; -*-Mode: Act-up; fill-column: 75; comment-column: 50; -*-
;;; Filename: choice.lisp

;; Choice unit test

;; These load commands will find the ACT-UP files
;; relative to the location of the present file:
(defvar *act-up-avoid-multiple-loading* nil)  ;; must be top-level to avoid compile-time errors in ccl.
(unless (find-symbol "actup-load")
  (let ((*act-up-avoid-multiple-loading* 'is-loading))
    (load (concatenate 'string (directory-namestring (or *load-truename* *compile-file-truename*)) "../load-act-up.lisp"))))

(actup-load "choice" "tutorial")

;; Architectural (ACT-R) Parameters
(setq *egs* 0.7)

;; Model parameter
(setq *positive-reward* 2.0)

;; -*-Mode: ACT-UP; fill-column: 75; comment-column: 50; -*-
