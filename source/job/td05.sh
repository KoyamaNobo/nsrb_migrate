#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/TSD010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'NS-KES' 'WK0064' '(1,17,C)' '((1,51),(@          @),(1,3))' '' '' '' '' '' '      手形　－－＞　財務　接続処理      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/TSD030 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSD030 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
