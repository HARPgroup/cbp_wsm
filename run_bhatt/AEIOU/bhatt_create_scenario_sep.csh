#!/bin/csh

set scenario = $argv[1]

source ../../config/control/script/$scenario.con

source ../fragments/set_tree


cd ../../run/datascripts/septic
csh SCENARIO.csh $SEPDATA $SEPFDF
