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
../exec/CSRT50 $USER_ID $JRCODE '10' 'TCM' 'WK0256' '((1,4,N),(148,14,C),(5,3,N))' '((1,192),(@          @),(1,54))' '' '' '' '(5,3,N,NE,@001@)' '' '    直送先マスター　Ｗチェックリスト    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMM230 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
