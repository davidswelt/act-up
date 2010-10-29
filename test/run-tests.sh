#!/bin/sh

echo "ACT-UP Regression tests"

echo "SBCL"
sbcl --eval '(progn (load "regressionsuite") (time (regression)) (quit))' 2>/dev/null

echo "CCL"
openmcl --eval '(progn (load "regressionsuite") (time (regression)) (quit))' 2>/dev/null

#echo "LispWorks"
#openmcl --eval '(progn (load "regressionsuite") (time (regression)))' 2>/dev/null