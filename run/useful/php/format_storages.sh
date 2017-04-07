#!/bin/sh

# loop through for all storage files in input directory

tree=`./get_tree.csh`

for i in `ls $tree/output/storage*.txt`; do
   php report_storages.php 65 "$tree/$i" "$tree/$i.csv"
done
   
