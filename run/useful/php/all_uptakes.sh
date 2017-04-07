#!/bin/sh

# loop through for all storage files in input directory
#lulist="nhy alf nal hom nho nlo nhi pas npa trp nhi nlo nhy for bar ext grs hvf puh pul trp urs hwm hyo lyw lwm"
#lulist="hvf puh pul trp urs"
lulist="hyw hwm hyo lyw lwm nhy alf nal hom nho nlo nhi pas npa trp for bar ext grs hvf puh pul trp urs"
path="/model/j510/output/"

if [ $1 == '--help' ];then
   echo "Usage: all_uptakes.sh [scenarioname] [landseg / all]"
else
   for i in $lulist;do
      ./results.sh $i $1 $2
   done
fi
