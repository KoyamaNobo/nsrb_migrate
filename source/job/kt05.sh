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
../exec/CSRT50 $USER_ID $JRCODE '10' 'URIRYR' 'WK0128' '((10,4,N),(2,8,N),(67,7,N))' '' '' '' '' '' '' '    ���Ӑ�ʁ@����E�l���`�[�@�⍇��    '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KHT050 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
