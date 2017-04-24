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
../exec/CSRT50 $USER_ID $JRCODE '10' 'NYURF' 'WK0128' '((67,3,N),(39,2,N),(9,4,N),(1,8,N),(51,8,C),(44,7,N))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '              ì¸ã‡ï[Å@î≠çs              '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HKD020 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
