#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HSHF1' 'WK0768' '((5,6,N),(1,4,N),(512,8,N))' '' '' '' '' '' '' '        品名別　発注入庫残明細表        '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBD750 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
