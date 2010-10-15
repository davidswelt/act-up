(defparameter source-files '("C:/emacs/usar-interface/ACT-UP/tutorial/choice.lisp" "C:/emacs/usar-interface/ACT-UP/tutorial/siegler.lisp" "C:/emacs/usar-interface/ACT-UP/tutorial/paired.lisp" "C:/emacs/usar-interface/ACT-UP/tutorial/fan.lisp" ))

(defun regression ()
  (loop for i in source-files
	do
	(progn
	  (load i)
	  (print i)
	  (let ((*standard-output* (make-string-output-stream)))  
	    (setq val (unit-test)))     
	  (if (> (first val) 0.90)
	      (if (> (second val) 0.1)
		  (print ' FAIL)
		(print 'OK))
	    (print 'FAIL)))))
	











