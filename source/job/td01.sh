#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0
../exec/TSD100 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TDTM' 'WK0256' '((1,1,N),(11,6,N),(3,4,N))' '((1,170),(@          @),(1,76))' '' '' '' '' '' '      受手・支払データ　入力リスト      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSD110 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
