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
../exec/CSRT50 $USER_ID $JRCODE '10' 'SNTRF' 'WK0064' '(92,2,N)' '((8,18),(57,5),(62,5),(67,8),(75,7),(85,6),(92,5),(128,1),(@' '         @))' '' '' '(7,1,N,NE,@9@)' '' '  óöï®ïîñÂíSìñìæà”êÊï Å@îÑè„èWåvñ‚çáÇπ  '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMT360 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
