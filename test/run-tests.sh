#!/bin/sh

echo "ACT-UP Regression tests"

find ../ -name *.dx64sl -delete
find ../ -name *.xfasl -delete
find ../ -name *.fasl -delete

echo "SBCL"
sbcl --eval '(progn (load "regressionsuite") (time (regression)) (quit))' 2>/dev/null

echo "CCL"
openmcl --eval '(progn (load "regressionsuite") (time (regression)) (quit))' 2>/dev/null

#echo "LispWorks"
#openmcl --eval '(progn (load "regressionsuite") (time (regression)))' 2>/dev/null