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
../exec/HMY060 $USER_ID $JRCODE 0 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'WK0064' 'WK0064' '(1,7,N)' '' '' '((8,4,P),(12,4,P),(16,4,P),(20,4,P),(24,4,P),(28,4,P),(32,4,' 'P),(36,4,P),(40,4,P),(44,4,P),(48,5,P))' '' '' '     年間品名サイズ別　出荷数明細表     '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMY080 $USER_ID $JRCODE 0 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
