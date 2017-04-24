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
../exec/CSRT50 $USER_ID $JRCODE '10' 'HTIM' 'WK0128' '((9,6,N),(8,1,N),(1,7,C))' '((1,85),(@          @),(1,33))' '' '' '' '' '' '        品名別　棚卸チェックリスト      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMN210 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
