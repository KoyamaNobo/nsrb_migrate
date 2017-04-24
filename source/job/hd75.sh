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
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '((8,8,N),(92,1,N))' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(1' '23,2),(125,1),(@      @))' '' '' '(7,1,N,NE,@9@)' '' '             —š•¨”Ì”„“úŒv•\             '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMD950 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
