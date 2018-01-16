#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TSAF' 'WK0064' '((1,10,N),(11,1,N))' '((1,32),(@          @),(1,22))' '' '' '' '' '' '            ëóã‡àƒì‡Å@ñæç◊ï\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSA050 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
