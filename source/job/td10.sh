#!/bin/tcsh
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
set ABORT=0
../exec/TSD150 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'TJSSF' 'WK0128' '((3,12,N),(1,2,N),(46,6,S))' '((1,102),(@          @),(1,16))' '' '' '' '' '' '      éËå`Å@Å|Å|ÅÑÅ@çwîÉÅ@ÇkÇhÇmÇj      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSD160 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
