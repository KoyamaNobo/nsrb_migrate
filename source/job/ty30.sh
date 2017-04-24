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
../exec/CSRT50 $USER_ID $JRCODE '10' 'UKETM' 'WK0256' '((18,4,N),(159,4,N),(34,4,N),(1,4,N))' '((1,170),(@          @),(1,76))' '' '' '' '' '' '          取引先別　受取手形帳          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0
../exec/TSY110 $USER_ID $JRCODE 0000 9999 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SHITM' 'WK0128' '((15,4,N),(121,4,N),(31,4,N),(1,4,N))' '' '' '' '' '' '' '          取引先別　支払手形帳          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSY210 $USER_ID $JRCODE 0000 9999 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
