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
../exec/CSRT50 $USER_ID $JRCODE '10' 'TM1' 'WK0512' '((177,2,N),(5,4,N))' '' '' '' '' '' '' '              ìæà”êÊÅ@ñºïÎ              '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKM530 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
