all: ACT-UP.html

act-up.lisp:


ACT-UP.html: act-up.lisp
	openmcl -l ../act-up.lisp -l ../lispdoc.lisp --eval '(progn (lispdoc:lispdoc-html "./" :act-up) (quit))'


symbols: act-up.lisp
	openmcl -l ../act-up.lisp -l ../lispdoc.lisp --eval '(progn (lispdoc:print-syms) (quit))'