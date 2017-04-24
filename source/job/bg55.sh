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
../exec/CSRT50 $USER_ID $JRCODE '10' 'JSSRF' 'WK0128' '((15,6,C),(59,6,C),(74,1,N),(11,4,N),(1,2,N),(53,4,C))' '((1,102),(@          @),(1,16))' '' '((21,9),(38,8))' '' '((15,6,C,NE,@999000@)A(16,1,C,EQ,@9@))' '' '       �d���x���ݐσt�@�C���@�W�v       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/KBG610 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((41,2,N),(33,6,N,D),(1,10,N),(27,6,N))' '' '' '' '' '' '' '            ���i�d���@���ו\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBG620 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'JSSRF' 'WK0128' '((15,6,C),(59,6,C),(74,1,N),(11,4,N),(1,2,N),(53,4,C))' '((1,102),(@          @),(1,16))' '' '((21,9),(38,8))' '' '((15,6,C,NE,@999000@)A(16,1,C,EQ,@9@))' '       �d���x���ݐσt�@�C���@�W�v       '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/KBG610 $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0064' '((41,2,N),(33,6,N,D),(1,6,N))' '' '' '((11,6),(17,9))' '' '' '' '            ���i�d���@���ו\            '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/KBG630 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
ENDJOB:
