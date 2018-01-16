#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HTIM' 'WK0128' '((9,6,N),(8,1,N),(1,7,C))' '((1,85),(@          @),(1,33))' '' '' '' '' '' '        品名別　棚卸チェックリスト      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMN210 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
