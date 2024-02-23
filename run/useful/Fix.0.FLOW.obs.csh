#!/bin/csh

if ( ${#argv} > 0 ) then

     set folder = $argv[1]

else

     echo 'Enter Name of the folder where OFLOW data are stored: '
     set folder = $<

endif

#find . -name "*.OFLOW" -print0 | xargs perl -pi -e 's/0.00000/111111/g'
#find $folder -type f -name "*.OFLOW" -print0 | xargs -0 sed -i 's/0.00000E+00/0.10000E-02,<,/g'

find $folder -type f -name "*.OFLOW" -print0 | xargs -0 sed -i 's/0.00000E+00/1.21000E-00,<,/g'

