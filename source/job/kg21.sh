#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((124,1,N),(10,9,C))' '' '' '' '' '' '' '    工品他　得意先品名別　売上集計表    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHG450 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
