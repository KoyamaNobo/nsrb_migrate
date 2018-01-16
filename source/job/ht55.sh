#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMG930 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
../exec/HMT550 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
