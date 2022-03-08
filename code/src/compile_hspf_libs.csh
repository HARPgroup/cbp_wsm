#!/bin/csh

####### by York Prado 10/2009
cd hspf/lib3.2/src/util
make clean 
make 
make install 
cd ../ 
cd wdm/
make clean
make 
make install
cd ../
cd adwdm/
make clean
make
make install

echo "Compiled all the lib3.2 files needed to compile the CBWM"

