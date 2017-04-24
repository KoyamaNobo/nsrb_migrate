#!/bin/tcsh
if (${#argv} == 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FN1 = ${JRCODE_PATH}${USER_ID};
set FN2 = ${ERROR_PATH}${USER_ID};
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((8,8,N),(7,1,N))' '' '' '((16,6),(22,6),(28,6),(34,6),(40,6),(46,6),(52,6),(58,6),(64' ',6),(70,6))' '' '' '            �q�ʁ@�I�����ו\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
