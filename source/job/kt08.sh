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
../exec/CSRT50 $USER_ID $JRCODE '10' 'KNHRYR' 'WK0064' '((11,5,C),(3,8,N),(1,2,N),(63,1,N))' '' '' '' '' '' '' '     ïiñºï Å@â¡ó∞ÅEîpãpì`ï[Å@ñ‚çáÇπ     '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHT310 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
