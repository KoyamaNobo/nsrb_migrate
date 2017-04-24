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
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((124,1,N),(10,9,C))' '' '' '' '' '' '' '    工品他　得意先品名別　売上集計表    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHG450 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
ENDJOB:
