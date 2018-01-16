#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HSMSF' 'WK0128' '((1,1,N),(9,8,N),(1,7,N))' '' '' '' '' '(7,1,N,NE,@7@)' '' '       出荷指図　売上未計上リスト       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMD810 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
