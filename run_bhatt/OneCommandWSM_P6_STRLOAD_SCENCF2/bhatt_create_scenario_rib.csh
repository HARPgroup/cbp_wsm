#!/bin/csh

set scenario = $argv[1]

source ../../config/control/script/$scenario.con

source ../fragments/set_tree


cd ../../run/datascripts/rib
csh SCENARIO.csh $RIBDATA $RIBFDF
