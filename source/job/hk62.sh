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
../exec/HKG200 $USER_ID $JRCODE 2
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CSRT50 $USER_ID $JRCODE '22' 'WK0256' 'WK0512' '((1,4,N),(72,3,N),(5,8,N),(13,8,N))' '((1,256),(@          @),(1,246))' '' '' '' '((1,4,N,EQ,@3241@)O(1,4,N,EQ,@3247@))' '' '      請求書サイズ別チェックリスト      '
source ../job/CRC_LIBRARY.sh
if ($JRCODE == 255) then
  goto ENDJOB
endif
if($ABORT == 1) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '11' 'WK0256' 'SHA' 'WK0512' 'ADD' '(1,256),(@          @),(1,246)' '' '' '' '(1,4,N,NE,@3241@)A(1,4,N,NE,@3247@)' '' '      請求書サイズ別チェックリスト      '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/CFLC10 $USER_ID $JRCODE '11' 'WK0512' 'SHA' 'WK0256' 'CRE' '(1,256)' '' '' '' '' '' '      請求書サイズ別チェックリスト      '
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if ($JRCODE == 255) then
  goto ENDJOB
endif
#**
set ABORT=0;
../exec/HKG650 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
if($JRCODE == 255) then
  goto ENDJOB
endif
set ABORT=0;
../exec/HKG660 $USER_ID $JRCODE 1
source ../job/CRC_LIBRARY.sh
if($ABORT == 1) then
  goto ENDJOB
endif
ENDJOB:
