#!/bin/ksh

cd ..
make
# if not fail
if [ $? -ne 0 ]; then
    echo "make failed"
    exit 1
fi
cd test
cp ../make ./
clear
./make -r -d P
