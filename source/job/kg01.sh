#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0
../exec/PRD350 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKG970 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
