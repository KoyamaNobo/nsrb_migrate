#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT058L $USER_ID $JRCODE 0 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
