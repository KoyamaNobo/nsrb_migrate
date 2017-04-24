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
../exec/CSRT50 $USER_ID $JRCODE '10' 'HSHF1' 'WK0768' '((512,8,N),(19,8,N),(1,10,N),(11,8,N))' '' '' '' '' '(520,6,N,EQ,@000000@)' '' '        î[ä˙ï Å@î≠íçì¸å…écñæç◊ï\        '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBT610 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
