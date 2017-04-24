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
../exec/CSRT50 $USER_ID $JRCODE '10' 'SKDF' 'WK0256' '((1,4,N),(72,3,N),(5,8,N),(14,7,N))' '((1,192),(@          @),(1,54))' '' '' '' '((1,4,N,EQ,@5000@)A(13,1,N,LE,@1@))' '' '        直送先日付別　請求明細表        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SKDF' 'WK0512' '((1,4,N),(83,08,C),(5,8,N),(14,7,N))' '((1,192),(@          @),(1,192),(1,118))' '' '' '' '((1,4,N,EQ,@9850@)A(13,1,N,LE,@1@))' '' '        直送先日付別　請求明細表        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '11' 'WK0512' 'SHA' 'WK0256' 'ADD' '' '' '' '' '' '' '        直送先日付別　請求明細表        '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '10' 'SKDF' 'WK0512' '((1,4,N),(72,3,N),(5,8,N),(14,7,N))' '((1,192),(@          @),(1,192),(1,118))' '' '' '' '((1,4,N,EQ,@4990@)A(13,1,N,LE,@1@))' '' '        直送先日付別　請求明細表        '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '11' 'WK0512' 'SHA' 'WK0256' 'ADD' '' '' '' '' '' '' '        直送先日付別　請求明細表        '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/HKG680 $USER_ID $JRCODE 0
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
ENDJOB:
