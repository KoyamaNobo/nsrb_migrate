#!/bin/tcsh
source ../job/RC_INIT.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'HKKYF' 'WK0064' '((1,1,N),(2,4,N),(37,6,N))' '((1,42),(@          @),(1,12))' '' '((6,7),(13,9),(22,7))' '' '' '' '  年間教育協議会会費　請求用品種別リスト'
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
../exec/HMK950 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
