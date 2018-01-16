#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0
../exec/KHY590 $USER_ID $JRCODE 5
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '((124,1,N),(10,9,C))' '' '' '' '' '' '' '    工品他　得意先品名別　売上集計表    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHY450 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
