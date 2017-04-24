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
../exec/CSRT50 $USER_ID $JRCODE '10' 'STRAN-3' 'WK0128' '(1,7,N)' '' '' '' '' '' '' '       ��������E�l���`�[�@�⍇��       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '01' 'SNTRF' 'SHA' 'WK0128' 'ADD' '' '' '' '' '' '' '       ��������E�l���`�[�@�⍇��       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/HMT650 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
