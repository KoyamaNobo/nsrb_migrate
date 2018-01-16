#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'NYURYR' 'WK0128' '((9,4,N),(1,8,N),(44,7,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '       ìæà”êÊï Å@ì¸ã‡ñæç◊Å@ñ‚çáÇπ       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKT410 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
