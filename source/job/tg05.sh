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
../exec/CSRT50 $USER_ID $JRCODE '10' 'TYBF' 'WK0064' '((1,4,N),(44,4,N),(7,4,N),(48,4,N),(13,4,N),(23,4,N))' '((1,51),(@          @),(1,3))' '' '' '' '' '' '           銀行別　割引手形帳           '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSG010 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
