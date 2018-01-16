#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'KHTNF' 'WK0064' '((15,3,N),(2,5,C),(1,1,N))' '((1,21),(@          @),(1,21),(1,12))' '' '' '' '' '' '    工品仕掛品　棚卸集計表　（場所）    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHY250 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
