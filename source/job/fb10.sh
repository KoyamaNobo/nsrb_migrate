#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0
../exec/FBP110 $USER_ID $JRCODE 0 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0
../job/ban21.sh $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/FBC010 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
