#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TTM' 'WK0128' '((94,2,N),(1,4,N))' '' '' '' '' '' '' '        担当得意先別　売上集計表        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMT310 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
