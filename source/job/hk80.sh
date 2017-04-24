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
../exec/CSRT50 $USER_ID $JRCODE '10' 'HKSRF' 'WK0064' '((1,8,N),(27,6,N))' '((1,32),(@          @),(1,22))' '' '((9,6),(15,8))' '' '' '' '      教育出荷集計　チェックリスト      '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
../exec/HMK050 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
