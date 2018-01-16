#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT051R $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
