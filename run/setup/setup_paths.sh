#!/bin/sh


# create a central location for the message.wdm
# probably should execute this relative to the current directory
# or a global path variable but for now here we are
sudo ln /opt/model/p53/cbp_wsm/code/src/hspf/lib3.2/lib_data/message.wdm /usr/local/lib/hspf/message.wdm -s
sudo ln /opt/model/p53/cbp_wsm/code/src/hspf/hspf11.1/bin/hspf_ICPRB /usr/local/bin/hspf_ICPRB -s
