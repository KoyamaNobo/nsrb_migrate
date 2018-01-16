#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMM110 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
