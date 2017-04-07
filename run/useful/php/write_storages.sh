#!/bin/sh

# run routine to parse all the .out files
# expects input of [luname] [scenarioname] [landseg]
# such as ./results.sh hwm newnut A42083
# or if all are desired
# such as ./results.sh hwm newnut all

if [ $1 == '--help' ];then
   echo "Usage: write_storages.sh [landuse] [scenarioname] [landseg / all] [scenarioid] [projectid]"
else 
   ./getstorages.sh $5 $4 $1 $2 $3 > ./land/$1/$2/storage_$1.txt
   # run the routine to pass the results to the database server
   php report_storages.php $5 ./land/$1/$2/storage_$1.txt ./land/$1/$2/storage_$1.csv $4 $2
fi
