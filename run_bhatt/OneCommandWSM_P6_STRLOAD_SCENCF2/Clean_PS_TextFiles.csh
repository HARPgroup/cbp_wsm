#!/bin/csh

# Written by Gopal Bhatt { gopal.bhatt@psu.edu }

#Read (a) dataversion and (b) filename
if (${#argv} > 0) then
    set dataversion = $argv[1]
    set filename   = $argv[2]
else
    echo 'Input PS Data Version: '
    set dataversion  = $<
    echo 'Input PS Ascii File Name: '
    set filename = $<
endif

if (${#argv} == 3) then
    set TempFileName = $argv[3]
else
    set TempFileName = tempfile
endif

cd ../../input/unformatted/point_source/$dataversion

#cp -u 
no | cp -pi $filename ${filename}.backup

find . -name $filename | xargs perl -pi -e 's/"//g'
find . -name $filename | xargs perl -pi -e 's/ //g'
#find . -name $filename | xargs perl -pi -e 's/,,,,,,,,,,/,0.000000000,0.000000000,0.000000000,0.000000000,0.000000000,0.000000000,0.000000000,0.000000000,0.000000000,0.000000000/g'
find . -name $filename | xargs perl -pi -e 's/,,/,0.000000000,/g'
find . -name $filename | xargs perl -pi -e 's/,,/,0.000000000,/g'
# find . -name $filename | xargs perl -pi -e 's/,$/,0.000000000/g'
#find . -name $filename | xargs perl -pi -e 's/,\n/,0.000000000\n/g'

#mv $filename tempFile
#sed 's/,$/,0.00/' tempFile > $filename
# rm tempFile

mv $filename $TempFileName
#awk 'BEGIN {RS=""}{gsub(/,\r/,",0.00",$0); print $0}' tempFile > $filename
awk 'BEGIN {RS=""}{gsub(",\r",",0.000000000\r",$0); print $0}' $TempFileName > $filename

rm $TempFileName
