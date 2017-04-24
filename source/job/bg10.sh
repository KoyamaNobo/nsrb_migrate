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
../exec/CSRT50 $USER_ID $JRCODE '10' 'JSSRF' 'WK0128' '(3,8,N)' '((1,102),(@          @),(1,16))' '' '' '' '(15,6,C,NE,@999000@)' '' '            �w���@�ޗ��W�v�\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/KBG110 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/KBG150 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/KBG160 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBG180 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
