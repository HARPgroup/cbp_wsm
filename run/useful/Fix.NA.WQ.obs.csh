#!/bin/csh

if ( ${#argv} > 0 ) then

     set folder = $argv[1]

else

     echo 'Enter Name of the folder where OFLOW data are stored: '
     set folder = $<

endif

#find . -name "*.OFLOW" -print0 | xargs perl -pi -e 's/0.00000/111111/g'
#find $folder -type f -name "*.OFLOW" -print0 | xargs -0 sed -i 's/0.00000E+00/0.10000E-02,<,/g'

find $folder/NH3X -type f -name "*.ONH3X" -print0 | xargs -0 sed -i 's/NA/12/'
find $folder/NH3X -type f -name "*.ONH3X" -print0 | xargs -0 sed -i 's/NA/00/'

find $folder/NO3X -type f -name "*.ONO3X" -print0 | xargs -0 sed -i 's/NA/12/'
find $folder/NO3X -type f -name "*.ONO3X" -print0 | xargs -0 sed -i 's/NA/00/'

find $folder/PO4X -type f -name "*.OPO4X" -print0 | xargs -0 sed -i 's/NA/12/'
find $folder/PO4X -type f -name "*.OPO4X" -print0 | xargs -0 sed -i 's/NA/00/'

find $folder/TOTN -type f -name "*.OTOTN" -print0 | xargs -0 sed -i 's/NA/12/'
find $folder/TOTN -type f -name "*.OTOTN" -print0 | xargs -0 sed -i 's/NA/00/'

find $folder/TOTP -type f -name "*.OTOTP" -print0 | xargs -0 sed -i 's/NA/12/'
find $folder/TOTP -type f -name "*.OTOTP" -print0 | xargs -0 sed -i 's/NA/00/'

find $folder/TSSX -type f -name "*.OTSSX" -print0 | xargs -0 sed -i 's/NA/12/'
find $folder/TSSX -type f -name "*.OTSSX" -print0 | xargs -0 sed -i 's/NA/00/'




