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
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRF' 'WK0128' '((14,5,C),(2,8,N))' '' '' '' '' '((1,1,N,EQ,@3@)O(1,1,N,EQ,@4@))' '' '         çHïiÅ@óaÇËéÛï•Å@ñ‚çáÇπ         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHT210 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
