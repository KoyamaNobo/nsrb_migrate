#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
../exec/HMG450 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((1,10,N),(11,6,N))' '' '' '((17,5),(22,5),(27,5))' '' '' '' '          óöï®Å@óaÇËéÛï•ñ‚çáÇπ          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMT510 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
