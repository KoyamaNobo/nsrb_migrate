#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'BANKM' 'WK0128' '((84,2,N),(5,16,C),(1,4,N))' '((1,85),(@          @),(1,33))' '' '' '' '' '' '         銀行マスター　コード表         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSM020 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
