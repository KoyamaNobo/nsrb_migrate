#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SMF' 'WK0128' '((93,2,N),(1,4,N),(5,8,N),(83,6,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '                êøãÅñæç◊ï\              '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKG060 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
