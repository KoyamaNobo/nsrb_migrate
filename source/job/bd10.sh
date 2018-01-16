#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/KBD520 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBD530 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
