#!/bin/tcsh
source ../job/RC_INIT.sh
../exec/PR205L $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
