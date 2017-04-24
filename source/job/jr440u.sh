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
../exec/PR441L $USER_ID $JRCODE 6607 6607 6612 6632 6634 7118 7120 7123 7126 7127 7129 8200 8202 9999
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
../exec/PR445L $USER_ID $JRCODE 66010000 99990000  
source ../job/CRC_LIBRARY.sh
ENDJOB:
