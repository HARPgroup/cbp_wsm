#!/bin/sh

stout="./land/$1/$2/$4"
plantout="./land/$1/$2/$5"
transout="./land/$1/$2/$6"

echo "landseg,landuse,year,Snh4,Sno3,Sslorn,Sdlorn,Ssrorn,Sdrorn,Spltn,nh4eof,no3eof,tneof,Fno3Sout,Fno3Uout,Fno3Gout,Fnh4Sout,Fnh4Uout,Fnh4Gout,FlornSout,FlornUout,FlornGout,FrornSout,FrornUout,FrornGout" > $stout

echo "landseg,landuse,syear,SLNretn,ULNretn,SBGretn,UBGretn,LBGretn,SAGnh4up,UAGnh4up,LAGnh4up,SBGnh4up,UBGnh4up,LBGnh4up,SAGno3up,UAGno3up,LAGno3up,SBGno3up,UBGno3up,LBGno3up" > $plantout

echo "landseg,landuse,syear,Ldntr,Gdntr,Sntr,Untr,Lntr,Gntr,Snh4imb,Unh4imb,Lnh4imb,Gnh4imb,Sognmin,Uognmin,Lognmin,Gognmin,Sno3imb,Uno3imb,Lno3imb,Gno3imb,Slbtoref,Ulbtoref,Llbtoref,Glbtoref" > $transout

# $1 = landuse
# $2 = scenario name (such as nut and newnut)
# $3 = land segment ( or 'all' for all)
# $4 = stoare & flux file
# $5 = plant uptake & return file
# $6 = N transform file

if [ $3 == 'all' ]; then
   files="../output/hspf/land/out/$1/$2/*.out"
else
   files="../output/hspf/land/out/$1/$2/$1$3.out"
fi

# must comment this since it goes to a file
#echo "Searching For $files "

for j in `ls $files`; do
   # must comment this since it goes to a file
  echo "Parsing $j "
   
   # pull apart the file name to get the relevant info
   # expects the file name convention to be static, in the form
   # of a 3 char lu name, 6 character landseg, which is 1 letter and 
   # a 5 char stcofips

   subshed=`echo $j | awk -F/ '{print(substr($5, 5, 5))}'`
   landuse=$1
   sceno=$2
   landseg=`echo $j | awk -F/ '{print(substr($5, 4, 6))}'`
   fname=$j

   # start year is assumed to be static, this is bad,
   # should probably pass this in as a parameter input
   # or simply grab it from within the file output
   syear=1984
   numyears=0
   lasttotal=0
   baseyear=$syear
   cumulativen=0
   # search the file for each instance of the section header NITR
   # fgrep -n send the format line_number:line_text,
   # awk, by default parses a line by spaces, but -F: asks it to 
   # parse by : (other delimiters can be used similarly)
   for i in `fgrep -n "*** NITR ***" $fname | awk -F: '{print int($1)}'`; do 
      sline=$i
      # the total line is located 14 lines from the header line
      eline=`expr $sline + 17`
      # there's gotta be a better way, but, pick out the desired line
      # by grabbing all lines up to and including the desired line
      # then sending it to tail, and grabbing the last line (the one
      #   we really wanted)

      # STORAGES
      storline=`head -n $eline $fname | tail -n 1`
      Snh4=`echo $storline | awk '{print $3}'`
      Sno3=`echo $storline | awk '{print $4}'`
      Sslorn=`echo $storline | awk '{print $5}'`
      Sdlorn=`echo $storline | awk '{print $6}'`
      Ssrorn=`echo $storline | awk '{print $7}'`
      Sdrorn=`echo $storline | awk '{print $8}'`
      Spltn=`echo $storline | awk '{print $9}'`
      
      # FLUXES
      Fnh4No=`expr $sline + 33`
      Fno3No=`expr $sline + 34`
      FlornNo=`expr $sline + 35`
      FrornNo=`expr $sline + 36`
      
      eofNo=`expr $sline + 42`
      eofline=`head -n $eofNo $fname | tail -n 1`      
      nh4eof=`echo $eofline | awk '{print $5}'`
      no3eof=`echo $eofline | awk '{print $6}'`
      tneof=`echo $eofline | awk '{print $8}'`

      Fno3line=`head -n $Fno3No $fname | tail -n 1`
      Fnh4line=`head -n $Fnh4No $fname | tail -n 1`
      Flornline=`head -n $FlornNo $fname | tail -n 1`
      Frornline=`head -n $FrornNo $fname | tail -n 1`

      Fno3Sout=`echo $Fno3line | awk '{print $2}'`
      Fno3Uout=`echo $Fno3line | awk '{print $6}'`
      Fno3Gout=`echo $Fno3line | awk '{print $9}'`

      Fnh4Sout=`echo $Fnh4line | awk '{print $4}'`
      Fnh4Uout=`echo $Fnh4line | awk '{print $8}'`
      Fnh4Gout=`echo $Fnh4line | awk '{print $11}'`

      FlornSout=`echo $Flornline | awk '{print $3}'`
      FlornUout=`echo $Flornline | awk '{print $7}'`
      FlornGout=`echo $Flornline | awk '{print $10}'`

      FrornSout=`echo $Frornline | awk '{print $3}'`
      FrornUout=`echo $Frornline | awk '{print $7}'`
      FrornGout=`echo $Frornline | awk '{print $10}'`

      # PLANT N RETURN TO LABILE ORGN
      LNreNo=`expr $sline + 49`
      BGreNo=`expr $sline + 57`

      LNreline=`head -n $LNreNo $fname | tail -n 1`
      BGreline=`head -n $BGreNo $fname | tail -n 1`

      SLNretn=`echo $LNreline | awk '{print $1}'`
      ULNretn=`echo $LNreline | awk '{print $2}'`

      SBGretn=`echo $BGreline | awk '{print $1}'`
      UBGretn=`echo $BGreline | awk '{print $2}'`
      LBGretn=`echo $BGreline | awk '{print $3}'`

      # PLANT UPTAKE
      AGnh4upNo=`expr $sline + 65`
      BGnh4upNo=`expr $sline + 69`
      AGno3upNo=`expr $sline + 73`
      BGno3upNo=`expr $sline + 77`

      AGnh4upline=`head -n $AGnh4upNo $fname | tail -n 1`
      BGnh4upline=`head -n $BGnh4upNo $fname | tail -n 1`
      AGno3upline=`head -n $AGno3upNo $fname | tail -n 1`
      BGno3upline=`head -n $BGno3upNo $fname | tail -n 1`

      SAGnh4up=`echo $AGnh4upline | awk '{print $1}'`
      UAGnh4up=`echo $AGnh4upline | awk '{print $2}'`
      LAGnh4up=`echo $AGnh4upline | awk '{print $3}'`

      SBGnh4up=`echo $BGnh4upline | awk '{print $1}'`
      UBGnh4up=`echo $BGnh4upline | awk '{print $2}'`
      LBGnh4up=`echo $BGnh4upline | awk '{print $3}'`

      SAGno3up=`echo $AGno3upline | awk '{print $1}'`
      UAGno3up=`echo $AGno3upline | awk '{print $2}'`
      LAGno3up=`echo $AGno3upline | awk '{print $3}'`

      SBGno3up=`echo $BGno3upline | awk '{print $1}'`
      UBGno3up=`echo $BGno3upline | awk '{print $2}'`
      LBGno3up=`echo $BGno3upline | awk '{print $3}'`

      # N TRANSFORMATION
      # denitrification line
      dntrNo=`expr $sline + 81`
      # nitrification line
      ntrNo=`expr $sline + 85`
      # NH3 immobilization line
      nh4imbNo=`expr $sline + 89`
      # ORNG minilization line
      ognminNo=`expr $sline + 93`
      # NO3 immobilization line
      no3imbNo=`expr $sline + 97`
      # Labile/Refer ORGN conversion
      lbtorefNo=`expr $sline + 101`

      dntrline=`head -n $dntrNo $fname | tail -n 1`
      ntrline=`head -n $ntrNo $fname | tail -n 1`    
      nh4imbline=`head -n $nh4imbNo $fname | tail -n 1`
      ognminline=`head -n $ognminNo $fname | tail -n 1`
      no3imbline=`head -n $no3imbNo $fname | tail -n 1`
      lbtorefline=`head -n $lbtorefNo $fname | tail -n 1`

      Ldntr=`echo $dntrline | awk '{print $3}'`
      Gdntr=`echo $dntrline | awk '{print $4}'`

      Sntr=`echo $ntrline | awk '{print $1}'`
      Untr=`echo $ntrline | awk '{print $2}'`
      Lntr=`echo $ntrline | awk '{print $3}'`
      Gntr=`echo $ntrline | awk '{print $4}'`

      Snh4imb=`echo $nh4imbline | awk '{print $1}'`
      Unh4imb=`echo $nh4imbline | awk '{print $2}'`
      Lnh4imb=`echo $nh4imbline | awk '{print $3}'`
      Gnh4imb=`echo $nh4imbline | awk '{print $4}'`

      Sognmin=`echo $ognminline | awk '{print $1}'`
      Uognmin=`echo $ognminline | awk '{print $2}'`
      Lognmin=`echo $ognminline | awk '{print $3}'`
      Gognmin=`echo $ognminline | awk '{print $4}'`

      Sno3imb=`echo $no3imbline | awk '{print $1}'`
      Uno3imb=`echo $no3imbline | awk '{print $2}'`
      Lno3imb=`echo $no3imbline | awk '{print $3}'`
      Gno3imb=`echo $no3imbline | awk '{print $4}'`

      Slbtoref=`echo $lbtorefline | awk '{print $1}'`
      Ulbtoref=`echo $lbtorefline | awk '{print $2}'`
      Llbtoref=`echo $lbtorefline | awk '{print $3}'`
      Glbtoref=`echo $lbtorefline | awk '{print $4}'`

      # print the output to the stdout
      echo $landseg,$landuse,$syear,$Snh4,$Sno3,$Sslorn,$Sdlorn,$Ssrorn,$Sdrorn,$Spltn,$nh4eof,$no3eof,$tneof,$Fno3Sout,$Fno3Uout,$Fno3Gout,$Fnh4Sout,$Fnh4Uout,$Fnh4Gout,$FlornSout,$FlornUout,$FlornGout,$FrornSout,$FrornUout,$FrornGout >> $stout

      echo $landseg,$landuse,$syear,$SLNretn,$ULNretn,$SBGretn,$UBGretn,$LBGretn,$SAGnh4up,$UAGnh4up,$LAGnh4up,$SBGnh4up,$UBGnh4up,$LBGnh4up,$SAGno3up,$UAGno3up,$LAGno3up,$SBGno3up,$UBGno3up,$LBGno3up >> $plantout

      echo $landseg,$landuse,$syear,$Ldntr,$Gdntr,$Sntr,$Untr,$Lntr,$Gntr,$Snh4imb,$Unh4imb,$Lnh4imb,$Gnh4imb,$Sognmin,$Uognmin,$Lognmin,$Gognmin,$Sno3imb,$Uno3imb,$Lno3imb,$Gno3imb,$Slbtoref,$Ulbtoref,$Llbtoref,$Glbtoref >> $transout
#      if [ $syear > $baseyear ];then
#         cumulativen=`echo "$cumulativen  $anuptk" | awk '{print($1 + $2)}'`
#         numyears=`expr $numyears + 1`
#      fi
     # echo "$cumulativen + $anuptk"
      syear=`expr $syear + 1`

      # stash the current year total for use next loop
#      lasttotal=$antotal
   done
#   avgn=`echo "$cumulativen $numyears" | awk '{print($1 / $2)}'`
#   echo "$landseg,totn,$avgn" >> $meanout
done


