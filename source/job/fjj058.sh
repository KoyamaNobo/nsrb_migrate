#!/bin/tcsh
source ../job/RC_INIT.sh
../exec/JT058L $USER_ID $JRCODE 1 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
