#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'UKETM' 'WK0256' '(153,6,N)' '((1,170),(@          @),(1,76))' '' '' '' '' '' '       割引手形　決済予定　一覧表       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TST460 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
