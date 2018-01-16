#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SKDKF' 'WK0256' '((62,2,N),(1,4,N),(21,6,N),(5,8,N))' '((1,192),(@          @),(1,54))' '' '' '' '' '' '     íSìñìæà”êÊïiéÌï Å@îÒêøãÅñæç◊ï\     '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKG280 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
