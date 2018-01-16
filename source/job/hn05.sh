#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/HMN310 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMD600 $USER_ID $JRCODE 3
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((91,2,N),(95,1,N),(87,3,N),(1,6,N),(96,1,N))' '' '' '' '' '' '' '       óöï®Å@ç›å…ñæç◊ï\Å@ÅoíIâµÅp       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMD620 $USER_ID $JRCODE 2 00 99 0 9 000 999 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
