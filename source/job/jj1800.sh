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
../exec/JT850U $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
if($JRCODE == 002) then
  goto A
endif
set ABORT=0;
set NORMAL=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((40,6,N),(25,4,N),(46,1,N))' '' '' '((47,4),(51,4),(55,4),(59,4),(63,4),(67,4),(71,4),(75,4),(79' ',4),(83,4))' '' '' '   　出荷確定未処理集計表（品名別）   　'
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($NORMAL == 1) then
  goto B
endif
A:
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '00' 'WK0256' '((40,6,N),(25,7,N),(46,1,N))' '' '' '((47,4),(51,4),(55,4),(59,4),(63,4),(67,4),(71,4),(75,4),(79' ',4),(83,4))' '' '' '   　出荷確定未処理集計表（品名別）   　'
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
B:
../exec/JT860L $USER_ID $JRCODE
source ../job/CRC_LIBRARY.sh
ENDJOB:
