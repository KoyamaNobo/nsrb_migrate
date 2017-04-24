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
../exec/CSRT50 $USER_ID $JRCODE '10' 'UKETM' 'WK0256' '((62,4,N),(153,6,N))' '((1,170),(@          @),(1,76))' '' '' '' '' '' '          äÑà¯éËå`Å@åàçœó\íËï\          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0
../exec/TSG210 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSG220 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
