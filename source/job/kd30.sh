#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0
../exec/KHD510 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/KHD530 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
../exec/KHD540 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
