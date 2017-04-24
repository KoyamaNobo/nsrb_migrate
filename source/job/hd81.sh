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
../exec/CSRT50 $USER_ID $JRCODE '10' 'HSMSF' 'WK0128' '((1,1,N),(9,8,N),(1,7,N))' '' '' '' '' '(7,1,N,NE,@7@)' '' '       出荷指図　売上未計上リスト       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMD810 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
