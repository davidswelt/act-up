#!/bin/sh

echo "ACT-UP Regression tests"

find ../ -name '*.dx64fsl' -delete
find ../ -name '*.xfasl' -delete
find ../ -name '*.fasl' -delete

echo "SBCL"
sbcl --eval '(progn (load "regressionsuite") (time (regression)) (quit))' 2>/dev/null

echo "CCL"
openmcl --eval '(progn (load "regressionsuite") (time (regression)) (quit))' 2>/dev/null

#echo "LispWorks"
#run the above delete commands (or this script).
#load regressionsuite.lisp, choose File->Compile and Load, type (time (regression)) into Listener window.

echo "LispWorks: load regressionsuite.lisp, choose File->Compile and Load, type (time (regression)) into Listener window."

