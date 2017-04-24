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
../exec/CSRT50 $USER_ID $JRCODE '10' 'SSRYF' 'WK0064' '((40,2,N),(1,10,N),(50,6,N))' '' '' '((11,7),(18,10),(28,10))' '' '' '' '担当得意先品種 年間売上集計 (ｳﾞｨｳﾞｪﾝﾃﾞｨ)'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY360 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
