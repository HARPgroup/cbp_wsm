#!/bin/sh

# run routine to parse all the .out files
# expects input of [luname] [scenarioname] [landseg]
# such as ./results.sh hwm newnut A42083
# or if all are desired
# such as ./results.sh hwm newnut all

prj=65
debug=0

if [ $5 == 1 ];then
   debug=1
fi

if [ $1 == '--help' ];then
   echo "Usage: results.sh [landuse] [scenarioname] [landseg / all] [scenarioid] [debug]"
else 
   if [ $3 == 'all' ];then
      lsegs=$3
   else
      lsegs=`./get_segs.csh $3.land`
   fi
   for m in $lsegs; do
      scen=$4
      echo "PRocessing $m \n"
      # get nitrogen
      ./get_uptake_totals.sh $prj $scen $1 $2 $m uptakes.txt mean_uptake.txt
      # get phosphorus
      ./get_uptake_phos.sh $prj $scen $1 $2 $m p_uptakes.txt mean_p.txt
      # run the routine to pass the results to the database server - nitrogen
      php report_uptakes.php $prj $scen $2 ./land/$1/$2/uptakes.txt $debug
      # run the routine to pass the results to the database server - phosphorus
      php report_uptakes.php $prj $scen $2 ./land/$1/$2/p_uptakes.txt $debug
      # now get storages
   #   ./getstorages.sh 65 1 $1 $2 $3 > storage.txt
    #  php report_storages.php 65 storage.txt
   done
fi
