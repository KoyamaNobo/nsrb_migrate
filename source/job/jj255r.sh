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
../exec/CSRT50 $USER_ID $JRCODE '10' 'NJZAI' 'WK0512' '(2,7,N)' '((1,341),(@          @),(1,161))' '' '' '' '(1,1,N,EQ,@9@)' '' '           é¿ç›å…ñæç◊Å@ñ‚çáÇπ           '
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT255R $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
