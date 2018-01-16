#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'THTM1' 'WK0064' '((35,2,N),(1,11,N))' '((1,42),(@          @),(1,12))' '' '' '' '' '' '      担当得意先品名別　単価リスト      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMM550 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
