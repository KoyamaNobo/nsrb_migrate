#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0
../exec/FBP950 $USER_ID $JRCODE 1 2
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0
../job/ban64.sh $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/FBP970 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
