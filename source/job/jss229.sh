#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JSS22L $USER_ID $JRCODE 9
source ../job/CRC_LIBRARY.sh
ENDJOB:
