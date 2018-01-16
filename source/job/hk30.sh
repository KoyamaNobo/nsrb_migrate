#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'NYURF' 'WK0128' '((39,2,N),(9,4,N),(1,8,N),(21,2,N),(24,8,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '        íSìñìæà”êÊï Å@ì¸ã‡ñæç◊ï\        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKG320 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
