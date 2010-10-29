#!/bin/sh

cd /tmp
rm -rf act-up-dist
mkdir act-up-dist
cd act-up-dist
git clone --depth 1 ~/MURI/usar-master

cd usar-master
REVID=`git rev-parse --short HEAD`
cd ..

mv usar-master/ACT-UP .
rm -rf usar-master
MAIN=`pwd`
cd ACT-UP
rm -rf test

sed -i "" -e 's/\(defparameter .act-up-version. \)\([0-9\.]*\)/\1 "'"$REVID"'" /' core/act-up.lisp

echo "ACT-UP Version $REVID " >README2
date >>README2
cat README >>README2
mv README2 README

cd doc
make
cd $MAIN
find . -name \*.dx64fsl -delete


zip ACT-UP.zip -r ACT-UP

echo "done."
echo `ls ACT-UP.zip`