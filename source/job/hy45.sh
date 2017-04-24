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
../exec/CSRT50 $USER_ID $JRCODE '10' 'HIYF' 'WK0128' '((92,2,N),(96,1,N),(88,3,N),(1,6,N),(7,6,N))' '((1,102),(@          @),(1,16))' '' '((13,6),(19,9),(28,7),(35,10),(45,8),(53,10),(63,6),(69,9),(' '78,10))' '' '' '        品種別　年間製品受払表          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HMG610 $USER_ID $JRCODE 1 0 1 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
../exec/HMG610 $USER_ID $JRCODE 1 1 1 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
