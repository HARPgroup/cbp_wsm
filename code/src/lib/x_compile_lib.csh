#!/bin/csh

####### section added by York Prado 10/2009
if( ! (-e /usr/bin/f77 || -e /bin/f77 || -e /usr/local/bin/f77) ) then
  echo "f77 command is not found"
  echo "Please make sure you have the fortran 77 compiler installed"
  exit 1
endif

# section below moved to compile_hspf_libs.csh
#cd hspf/lib3.2/src/util
#make clean 
#make 
#make install 
#cd ../ 
#cd wdm/
#make clean
#make 
#make install
#cd ../
#cd adwdm/
#make clean
##make
#make install
#echo "Compiled all the lib3.2 files needed to compile the CBWM"
#cd ../../../../ 

######### end York Prado section

cd dsn/
f77 -mcmodel=medium -fbounds-check -c x_dsn_utils.f

cd ../util
rm ../x_util_lib.a
./x_compile
cd ../get
rm ../x_get_lib.a
./x_compile
cd ../tty/
gcc -c -o ../ttyux.o ttyux.c

cd ../../
  