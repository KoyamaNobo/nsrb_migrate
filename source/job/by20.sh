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
../exec/CSRT50 $USER_ID $JRCODE '10' 'JTM' 'WK0128' '((73,2,N),(1,6,N))' '((1,85),(@          @),(1,33))' '' '' '' '' '' '         çﬁóøÅ@íIâµç∑äzÅ@ñæç◊ï\         '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBY020 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
