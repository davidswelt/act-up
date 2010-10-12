all: ACT-UP.html

act-up.lisp:


ACT-UP.html: act-up.lisp
	openmcl -l ../load-act-up.lisp -l ../util/lispdoc.lisp --eval '(progn (lispdoc:lispdoc-html "./" :act-up) (quit))'


symbols: act-up.lisp
	openmcl -l ../load-act-up.lisp -l ../util/lispdoc.lisp --eval '(progn (lispdoc:print-syms) (quit))'