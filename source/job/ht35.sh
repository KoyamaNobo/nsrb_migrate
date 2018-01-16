#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SMF' 'WK0128' '(1,12,N)' '((1,102),(@          @),(1,16))' '' '' '' '' '' '       ìæà”êÊï Å@êøãÅñæç◊Å@ñ‚çáÇπ       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKT310 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
