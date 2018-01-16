#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/PR610U $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((33,1,C),(26,4,C),(1,8,C),(9,17,C))' '' '' '' '' '' '' '           è¡îÔê≈Å@êUë÷ñæç◊ï\           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/PR605L $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
