#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JTN00U $USER_ID $JRCODE 6
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
