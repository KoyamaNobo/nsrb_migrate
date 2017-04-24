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
../exec/CSRT50 $USER_ID $JRCODE '10' 'SHITM' 'WK0128' '((15,4,N),(121,4,N),(31,4,N),(125,4,N),(37,4,N))' '' '' '' '' '' '' '          支払手形　期日　抽出          '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSG900 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((5,4,N),(1,4,N))' '' '' '' '' '' '' '      期日別支払手形　サイト一覧表      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/TSG950 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
