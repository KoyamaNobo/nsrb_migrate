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
source ../job/db_alloc.sh "hy14.sh" "WK0256"
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto J
endif
#***
set ABORT=0;
../exec/HMY950 $USER_ID $JRCODE 
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
/user/local/bin/CSRT50 '13' 'WK0256' 'FDU' 'W232' '((1,6,N),(55,7,N))' '(1,232)' '' '' '' '' '' '  品種月別サイズ製品受払ファイル　作成  '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#***
set ABORT=0;
source ../job/db_alloc.sh "hk14.sh" "WK0256"
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
