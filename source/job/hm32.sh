#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TM1' 'WK0512' '((177,2,N),(5,4,N))' '' '' '' '' '' '' '              ìæà”êÊÅ@ñºïÎ              '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKM530 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
