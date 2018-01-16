#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JT795U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((105,6,C),(24,4,N),(2,7,N),(9,8,N),(17,7,N))' '' '' '' '' '' '' '　　　　　　　　受注台帳　　　　　　　　'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT345L $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
