#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((43,2,N),(8,2,N))' '' '' '' '' '' '' '    用途区分日付別　売上金額　問合せ    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHT610 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
