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
../exec/JT550U $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto A
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((9,8,N),(1,6,N),(7,1,N))' '' '' '' '' '' '' '　　　　　出荷確定未処理リスト　　　　　'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT560L $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
A:
set JRCODE=000;
set ABORT=0;
../exec/JT550U $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((9,8,N),(1,6,N),(7,1,N))' '' '' '' '' '' '' '　　　　　送り状未更新リスト　　　　　　'
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
../exec/JT560L $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
ENDJOB:
