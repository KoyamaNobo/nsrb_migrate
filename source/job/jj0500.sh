#!/bin/tcsh
source ../job/RC_INIT.sh
../exec/JT050L $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
