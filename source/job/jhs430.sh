#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/JHS43U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0128' '(9,24,C)' '' '' '((33,4),(37,4),(41,4),(45,4),(49,4),(53,4),(57,4),(61,4),(65' ',4),(69,4))' '' '' '  赤ちゃん本舗指図変換ワーク作成      　'
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/JHS44L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
