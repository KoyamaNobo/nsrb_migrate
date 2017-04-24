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
../exec/CSRT50 $USER_ID $JRCODE '10' 'BANKM' 'WK0128' '((84,2,N),(5,16,C),(1,4,N))' '((1,85),(@          @),(1,33))' '' '' '' '' '' '         銀行マスター　コード表         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSM020 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
