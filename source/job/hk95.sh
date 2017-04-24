#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
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
