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
../exec/CSRT50 $USER_ID $JRCODE '10' 'TTMYR' 'WK0128' '((1,4,N),(123,6,N))' '' '' '' '' '' '' '得意先月別　年間売上金額明細表　（経理用'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY460 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
