#!/bin/tcsh
source ../job/RC_INIT.sh
../exec/KHD020 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
