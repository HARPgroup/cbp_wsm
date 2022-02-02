#!/bin/bash

ip=$1

if [ -z "$ip" ]; then
  ip="/usr/local/bin"
fi

cp $PWD/bin/find_config $ip/find_config
cp $PWD/bin/hspf_config $ip/hspf_config
cp $PWD/bin/cbp $ip/cbp
cp $PWD/bin/hspf.config.default $PWD/hspf.config
