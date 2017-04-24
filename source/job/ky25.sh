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
../exec/CSRT50 $USER_ID $JRCODE '10' 'KHTNF' 'WK0064' '((15,3,N),(2,5,C),(1,1,N))' '((1,21),(@          @),(1,21),(1,12))' '' '' '' '' '' '    工品仕掛品　棚卸集計表　（場所）    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHY250 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
