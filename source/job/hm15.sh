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
../exec/CSRT50 $USER_ID $JRCODE '10' 'THTM1' 'WK0064' '((35,2,N),(1,11,N))' '((1,42),(@          @),(1,12))' '' '' '' '' '' '      担当得意先品名別　単価リスト      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMM550 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
